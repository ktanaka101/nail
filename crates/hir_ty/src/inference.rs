mod environment;
mod error;
mod type_scheme;
mod type_unifier;
mod types;

use std::collections::HashMap;

use environment::Environment;
pub use error::InferenceError;
use indexmap::IndexMap;
pub use type_scheme::TypeScheme;
pub use types::Monotype;

use self::{
    environment::Context,
    type_unifier::{TypeUnifier, UnifyPurpose},
};
use crate::HirTyMasterDatabase;

/// Pod全体の型推論を行う
pub fn infer_pods(db: &dyn HirTyMasterDatabase, pods: &hir::Pods) -> InferenceResult {
    let mut signature_by_function = HashMap::<hir::Function, Signature>::new();
    for (hir_file, function) in pods.root_pod.all_functions(db) {
        let signature = lower_signature(db, hir_file, function);
        signature_by_function.insert(function, signature);
    }

    let mut body_result_by_function = IndexMap::<hir::Function, InferenceBodyResult>::new();
    for (hir_file, function) in pods.root_pod.all_functions(db) {
        let env = Environment::new();
        let infer_body = InferBody::new(db, pods, hir_file, function, &signature_by_function, env);
        let infer_body_result = infer_body.infer_body();

        body_result_by_function.insert(function, infer_body_result);
    }

    InferenceResult {
        signature_by_function,
        inference_body_result_by_function: body_result_by_function,
    }
}

#[salsa::tracked]
pub(crate) fn lower_signature(
    db: &dyn HirTyMasterDatabase,
    hir_file: hir::HirFile,
    function: hir::Function,
) -> Signature {
    let params = function
        .params(db)
        .iter()
        .map(|param| {
            let param_data = param.data(hir_file.db(db));
            lower_type(&param_data.ty)
        })
        .collect::<Vec<_>>();

    let return_type = lower_type(&function.return_type(db));

    Signature::new(db, params, return_type)
}

fn lower_type(ty: &hir::Type) -> Monotype {
    match ty {
        hir::Type::Integer => Monotype::Integer,
        hir::Type::String => Monotype::String,
        hir::Type::Char => Monotype::Char,
        hir::Type::Boolean => Monotype::Bool,
        hir::Type::Unit => Monotype::Unit,
        hir::Type::Unknown => Monotype::Unknown,
    }
}

/// 関数シグネチャ
#[salsa::tracked]
pub struct Signature {
    /// 関数のパラメータ
    #[return_ref]
    pub params: Vec<Monotype>,
    /// 関数の戻り値
    pub return_type: Monotype,
}

/// 関数内を型推論します。
pub(crate) struct InferBody<'a> {
    db: &'a dyn HirTyMasterDatabase,
    pods: &'a hir::Pods,
    hir_file: hir::HirFile,
    function: hir::Function,
    signature: Signature,

    unifier: TypeUnifier,
    cxt: Context,

    /// 環境のスタック
    ///
    /// スコープを入れ子にするために使用しています。
    /// スコープに入る時にpushし、スコープから抜ける時はpopします。
    env_stack: Vec<Environment>,
    signature_by_function: &'a HashMap<hir::Function, Signature>,

    /// 推論結果の式の型を記録するためのマップ
    type_by_expr: HashMap<hir::ExprId, Monotype>,
}
impl<'a> InferBody<'a> {
    pub(crate) fn new(
        db: &'a dyn HirTyMasterDatabase,
        pods: &'a hir::Pods,
        hir_file: hir::HirFile,
        function: hir::Function,
        signature_by_function: &'a HashMap<hir::Function, Signature>,
        env: Environment,
    ) -> Self {
        InferBody {
            db,
            pods,
            hir_file,
            function,
            signature: *signature_by_function.get(&function).unwrap(),

            unifier: TypeUnifier::new(),
            cxt: Context::default(),
            env_stack: vec![env],
            signature_by_function,
            type_by_expr: HashMap::new(),
        }
    }

    pub(crate) fn infer_body(mut self) -> InferenceBodyResult {
        let hir::Expr::Block(body) = self.hir_file.function_body_by_function(self.db, self.function).unwrap() else { panic!("Should be Block.") };
        let mut has_never = false;
        for stmt in &body.stmts {
            let is_never = self.infer_stmt(stmt);
            if is_never {
                has_never = true;
            }
        }

        if let Some(tail) = &body.tail {
            let ty = self.infer_expr(*tail);
            let ty = if has_never { Monotype::Never } else { ty };
            self.unifier.unify(
                &self.signature.return_type(self.db),
                &ty,
                &UnifyPurpose::ReturnValue {
                    expected_signature: self.signature,
                    expected_function: self.function,
                    found_value: Some(*tail),
                },
            );
        } else {
            let ty = if has_never {
                Monotype::Never
            } else {
                Monotype::Unit
            };
            self.unifier.unify(
                &self.signature.return_type(self.db),
                &ty,
                &UnifyPurpose::ReturnValue {
                    expected_signature: self.signature,
                    expected_function: self.function,
                    found_value: None,
                },
            );
        };

        InferenceBodyResult {
            type_by_expr: self.type_by_expr,
            errors: self.unifier.errors,
        }
    }

    fn infer_type(&mut self, ty: &hir::Type) -> Monotype {
        match ty {
            hir::Type::Integer => Monotype::Integer,
            hir::Type::String => Monotype::String,
            hir::Type::Char => Monotype::Char,
            hir::Type::Boolean => Monotype::Bool,
            hir::Type::Unit => Monotype::Unit,
            hir::Type::Unknown => Monotype::Unknown,
        }
    }

    fn infer_stmt(&mut self, stmt: &hir::Stmt) -> bool {
        let ty = match stmt {
            hir::Stmt::VariableDef { name: _, value } => {
                let ty = self.infer_expr(*value);
                let ty_scheme = TypeScheme::new(ty.clone());
                self.mut_current_scope().insert(*value, ty_scheme);
                ty
            }
            hir::Stmt::ExprStmt {
                expr,
                has_semicolon: _,
            } => self.infer_expr(*expr),
            hir::Stmt::Item { .. } => return false,
        };

        ty == Monotype::Never
    }

    fn infer_symbol(&mut self, symbol: &hir::Symbol, source_expr: hir::ExprId) -> Monotype {
        match symbol {
            hir::Symbol::Param { name: _, param } => {
                let param = param.data(self.hir_file.db(self.db));
                self.infer_type(&param.ty)
            }
            hir::Symbol::Local { name: _, expr } => {
                let ty_scheme = self.current_scope().get(expr).cloned();
                if let Some(ty_scheme) = ty_scheme {
                    ty_scheme.instantiate(&mut self.cxt)
                } else {
                    panic!("Unbound variable {symbol:?}");
                }
            }
            hir::Symbol::Missing { path } => {
                let resolution_status = self.pods.resolution_map.item_by_symbol(path).unwrap();
                match self.resolve_resolution_status(resolution_status) {
                    Some(item) => match item {
                        hir::Item::Function(function) => {
                            let signature = self.signature_by_function.get(&function).unwrap();
                            Monotype::Function(*signature)
                        }
                        hir::Item::Module(module) => {
                            self.unifier.add_error(InferenceError::ModuleAsExpr {
                                found_module: module,
                                found_expr: source_expr,
                            });
                            Monotype::Unknown
                        }
                        hir::Item::UseItem(_) => unreachable!("UseItem should be resolved."),
                    },
                    None => Monotype::Unknown,
                }
            }
        }
    }

    fn resolve_resolution_status(
        &self,
        resolution_status: hir::ResolutionStatus,
    ) -> Option<hir::Item> {
        match resolution_status {
            hir::ResolutionStatus::Unresolved | hir::ResolutionStatus::Error => None,
            hir::ResolutionStatus::Resolved { path: _, item } => match item {
                hir::Item::Function(_) | hir::Item::Module(_) => Some(item),
                hir::Item::UseItem(use_item) => {
                    let resolution_status = self.pods.resolution_map.item_by_use_item(&use_item)?;
                    self.resolve_resolution_status(resolution_status)
                }
            },
        }
    }

    fn infer_expr(&mut self, expr_id: hir::ExprId) -> Monotype {
        let expr = expr_id.lookup(self.hir_file.db(self.db));
        let ty = match expr {
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Integer(_) => Monotype::Integer,
                hir::Literal::String(_) => Monotype::String,
                hir::Literal::Char(_) => Monotype::Char,
                hir::Literal::Bool(_) => Monotype::Bool,
            },
            hir::Expr::Missing => Monotype::Unknown,
            hir::Expr::Symbol(symbol) => self.infer_symbol(symbol, expr_id),
            hir::Expr::Call {
                callee,
                args: call_args,
            } => {
                let callee_ty = self.infer_symbol(callee, expr_id);
                match callee_ty {
                    Monotype::Integer
                    | Monotype::Bool
                    | Monotype::Unit
                    | Monotype::Char
                    | Monotype::String
                    | Monotype::Never
                    | Monotype::Unknown
                    | Monotype::Variable(_) => {
                        self.unifier.add_error(InferenceError::NotCallable {
                            found_callee_ty: callee_ty,
                            found_callee_symbol: callee.clone(),
                            found_callee_expr: expr_id,
                        });
                        Monotype::Unknown
                    }
                    Monotype::Function(signature) => {
                        let call_args_ty = call_args
                            .iter()
                            .map(|arg| self.infer_expr(*arg))
                            .collect::<Vec<_>>();

                        if call_args_ty.len() != signature.params(self.db).len() {
                            self.unifier
                                .add_error(InferenceError::MismatchedCallArgCount {
                                    expected_callee_arg_count: signature.params(self.db).len(),
                                    found_arg_count: call_args_ty.len(),
                                    found_expr: expr_id,
                                });
                        } else {
                            for (((arg_pos, call_arg), call_arg_ty), signature_arg_ty) in call_args
                                .iter()
                                .enumerate()
                                .zip(call_args_ty)
                                .zip(signature.params(self.db))
                            {
                                self.unifier.unify(
                                    signature_arg_ty,
                                    &call_arg_ty,
                                    &UnifyPurpose::CallArg {
                                        found_arg_expr: *call_arg,
                                        callee_signature: signature,
                                        arg_pos,
                                    },
                                );
                            }
                        }

                        // 引数の数が異なったとしても、関数の戻り値は返す。
                        signature.return_type(self.db)
                    }
                }
            }
            hir::Expr::Binary { op, lhs, rhs } => match op {
                ast::BinaryOp::Add(_)
                | ast::BinaryOp::Sub(_)
                | ast::BinaryOp::Mul(_)
                | ast::BinaryOp::Div(_) => {
                    let lhs_ty = self.infer_expr(*lhs);
                    let rhs_ty = self.infer_expr(*rhs);
                    self.unifier.unify(
                        &Monotype::Integer,
                        &lhs_ty,
                        &UnifyPurpose::BinaryInteger {
                            found_expr: *lhs,
                            op: op.clone(),
                        },
                    );
                    self.unifier.unify(
                        &Monotype::Integer,
                        &rhs_ty,
                        &UnifyPurpose::BinaryInteger {
                            found_expr: *rhs,
                            op: op.clone(),
                        },
                    );

                    Monotype::Integer
                }
                ast::BinaryOp::Equal(_)
                | ast::BinaryOp::GreaterThan(_)
                | ast::BinaryOp::LessThan(_) => {
                    let lhs_ty = self.infer_expr(*lhs);
                    let rhs_ty = self.infer_expr(*rhs);
                    self.unifier.unify(
                        &lhs_ty,
                        &rhs_ty,
                        &UnifyPurpose::BinaryCompare {
                            found_compare_from_expr: *lhs,
                            found_compare_to_expr: *rhs,
                            op: op.clone(),
                        },
                    );

                    Monotype::Bool
                }
            },
            hir::Expr::Unary { op, expr } => match op {
                ast::UnaryOp::Neg(_) => {
                    let expr_ty = self.infer_expr(*expr);
                    self.unifier.unify(
                        &Monotype::Integer,
                        &expr_ty,
                        &UnifyPurpose::Unary {
                            found_expr: *expr,
                            op: op.clone(),
                        },
                    );

                    Monotype::Integer
                }
                ast::UnaryOp::Not(_) => {
                    let expr_ty = self.infer_expr(*expr);
                    self.unifier.unify(
                        &Monotype::Bool,
                        &expr_ty,
                        &UnifyPurpose::Unary {
                            found_expr: *expr,
                            op: op.clone(),
                        },
                    );

                    Monotype::Bool
                }
            },
            hir::Expr::Block(block) => {
                self.entry_scope();

                let mut has_never = false;
                for stmt in &block.stmts {
                    let is_never = self.infer_stmt(stmt);
                    if is_never {
                        has_never = true;
                    }
                }

                let ty = if let Some(tail) = &block.tail {
                    self.infer_expr(*tail)
                } else {
                    // 最後の式がない場合は Unit として扱う
                    Monotype::Unit
                };
                if ty == Monotype::Never {
                    has_never = true;
                }

                self.exit_scope();

                if has_never {
                    Monotype::Never
                } else {
                    ty
                }
            }
            hir::Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_ty = self.infer_expr(*condition);
                self.unifier.unify(
                    &Monotype::Bool,
                    &condition_ty,
                    &UnifyPurpose::IfCondition {
                        found_condition_expr: *condition,
                    },
                );

                let then_ty = self.infer_expr(*then_branch);
                let mut else_ty: Option<Monotype> = None;
                if let Some(else_branch) = else_branch {
                    let ty = self.infer_expr(*else_branch);
                    self.unifier.unify(
                        &then_ty,
                        &ty,
                        &UnifyPurpose::IfThenElseBranch {
                            found_then_branch_expr: *then_branch,
                            found_else_branch_expr: *else_branch,
                        },
                    );
                    else_ty = Some(ty);
                } else {
                    // elseブランチがない場合は Unit として扱う
                    self.unifier.unify(
                        &Monotype::Unit,
                        &then_ty,
                        &UnifyPurpose::IfThenOnlyBranch {
                            found_then_branch_expr: *then_branch,
                        },
                    );
                }

                if let Some(else_ty) = else_ty {
                    if then_ty == Monotype::Never {
                        else_ty
                    } else {
                        then_ty
                    }
                } else if then_ty == Monotype::Never {
                    Monotype::Never
                } else {
                    then_ty
                }
            }
            hir::Expr::Return { value } => {
                if let Some(return_value) = value {
                    let ty = self.infer_expr(*return_value);
                    self.unifier.unify(
                        &self.signature.return_type(self.db),
                        &ty,
                        &UnifyPurpose::ReturnExpr {
                            expected_signature: self.signature,
                            found_return_expr: Some(*return_value),
                            found_return: expr_id,
                        },
                    );
                } else {
                    // 何も指定しない場合は Unit を返すものとして扱う
                    self.unifier.unify(
                        &self.signature.return_type(self.db),
                        &Monotype::Unit,
                        &UnifyPurpose::ReturnExpr {
                            expected_signature: self.signature,
                            found_return_expr: None,
                            found_return: expr_id,
                        },
                    );
                }

                // return自体の戻り値は Never として扱う
                Monotype::Never
            }
            hir::Expr::Loop { block } => {
                self.infer_expr(*block);

                // TODO: まだbreakがないので、Neverとして扱う
                Monotype::Never
            }
            hir::Expr::Continue => todo!(),
            hir::Expr::Break { .. } => todo!(),
        };

        self.type_by_expr.insert(expr_id, ty.clone());

        ty
    }

    fn entry_scope(&mut self) {
        let env = self.env_stack.last().unwrap().with();
        self.env_stack.push(env);
    }

    fn exit_scope(&mut self) {
        self.env_stack.pop();
    }

    fn mut_current_scope(&mut self) -> &mut Environment {
        self.env_stack.last_mut().unwrap()
    }

    fn current_scope(&self) -> &Environment {
        self.env_stack.last().unwrap()
    }
}

/// Pod全体の型推論結果
#[derive(Debug)]
pub struct InferenceResult {
    pub(crate) signature_by_function: HashMap<hir::Function, Signature>,
    pub(crate) inference_body_result_by_function: IndexMap<hir::Function, InferenceBodyResult>,
}

/// 関数内の型推論結果
#[derive(Debug)]
pub struct InferenceBodyResult {
    pub(crate) type_by_expr: HashMap<hir::ExprId, Monotype>,
    pub(crate) errors: Vec<InferenceError>,
}
impl InferenceBodyResult {
    pub fn type_by_expr(&self, expr: hir::ExprId) -> Option<&Monotype> {
        self.type_by_expr.get(&expr)
    }

    pub fn errors(&self) -> &Vec<InferenceError> {
        &self.errors
    }
}
