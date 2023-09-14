use std::{
    collections::{HashMap, HashSet},
    ops::Sub,
};

use super::{type_scheme::TypeScheme, type_unifier::TypeUnifier, types::Monotype};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub params: Vec<Monotype>,
    pub return_type: Monotype,
}

pub(crate) struct InferBody<'a> {
    db: &'a dyn hir::HirMasterDatabase,
    pods: &'a hir::Pods,
    hir_file: hir::HirFile,
    function: hir::Function,
    signature: &'a Signature,

    unifier: TypeUnifier,
    cxt: Context,

    env_stack: Vec<Environment>,
    signature_by_function: &'a HashMap<hir::Function, Signature>,
    type_by_expr: HashMap<hir::ExprId, Monotype>,
}
impl<'a> InferBody<'a> {
    pub(crate) fn new(
        db: &'a dyn hir::HirMasterDatabase,
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
            signature: signature_by_function.get(&function).unwrap(),

            unifier: TypeUnifier::new(),
            cxt: Context::default(),
            env_stack: vec![env],
            signature_by_function,
            type_by_expr: HashMap::new(),
        }
    }

    pub(crate) fn infer_body(mut self) -> InferenceBodyResult {
        let hir::Expr::Block(body) = self.hir_file.function_body_by_function(self.db, self.function).unwrap() else { panic!("Should be Block.") };
        for stmt in &body.stmts {
            self.infer_stmt(stmt);
        }

        let ty = if let Some(tail) = &body.tail {
            self.infer_expr(*tail)
        } else {
            Monotype::Unit
        };
        dbg!(&self.signature.return_type);
        self.unifier.unify(&ty, &self.signature.return_type);

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

    fn infer_stmt(&mut self, stmt: &hir::Stmt) {
        match stmt {
            hir::Stmt::VariableDef { name, value } => {
                let ty = self.infer_expr(*value);
                let ty_scheme = TypeScheme::new(ty);
                self.mut_current_scope().bindings.insert(*name, ty_scheme);
            }
            hir::Stmt::ExprStmt {
                expr,
                has_semicolon: _,
            } => {
                self.infer_expr(*expr);
            }
            hir::Stmt::Item { .. } => (),
        }
    }

    fn infer_symbol(&mut self, symbol: &hir::Symbol) -> Monotype {
        match symbol {
            hir::Symbol::Param { name: _, param } => {
                let param = param.data(self.hir_file.db(self.db));
                self.infer_type(&param.ty)
            }
            hir::Symbol::Local { name, expr: _ } => {
                let ty_scheme = self.current_scope().bindings.get(name).cloned();
                if let Some(ty_scheme) = ty_scheme {
                    ty_scheme.instantiate(&mut self.cxt)
                } else {
                    panic!("Unbound variable {symbol:?}");
                }
            }
            hir::Symbol::Missing { path } => {
                let item = self.pods.resolution_map.item_by_symbol(path).unwrap();
                match item {
                    hir::ResolutionStatus::Unresolved | hir::ResolutionStatus::Error => {
                        // 解決できないエラーを追加
                        Monotype::Unknown
                    }
                    hir::ResolutionStatus::Resolved { path, item } => {
                        match item {
                            hir::Item::Function(function) => {
                                let signature = self.signature_by_function.get(&function);
                                if let Some(signature) = signature {
                                    Monotype::Function {
                                        args: signature.params.clone(),
                                        return_type: Box::new(signature.return_type.clone()),
                                    }
                                } else {
                                    unreachable!("Function signature should be resolved.")
                                }
                            }
                            hir::Item::Module(_) => {
                                // モジュールを型推論使用としているエラーを追加
                                Monotype::Unknown
                            }
                            hir::Item::UseItem(_) => {
                                // 使用宣言を型推論使用としているエラーを追加
                                Monotype::Unknown
                            }
                        }
                    }
                }
            }
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
            hir::Expr::Symbol(symbol) => self.infer_symbol(symbol),
            hir::Expr::Call {
                callee,
                args: call_args,
            } => {
                let ty = self.infer_symbol(callee);
                match ty {
                    Monotype::Integer
                    | Monotype::Bool
                    | Monotype::Unit
                    | Monotype::Char
                    | Monotype::String
                    | Monotype::Never
                    | Monotype::Unknown
                    | Monotype::Variable(_) => {
                        // TODO: 関数ではないものを呼び出そうとしているエラーを追加
                        Monotype::Unknown
                    }
                    Monotype::Function { args, return_type } => {
                        let call_args_ty = call_args
                            .iter()
                            .map(|arg| self.infer_expr(*arg))
                            .collect::<Vec<_>>();

                        if call_args_ty.len() != args.len() {
                            // TODO: 引数の数が異なるエラーを追加
                            Monotype::Unknown
                        } else {
                            for (call_arg, arg) in call_args_ty.iter().zip(args.iter()) {
                                self.unifier.unify(call_arg, arg);
                            }

                            *return_type
                        }
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
                    self.unifier.unify(&Monotype::Integer, &lhs_ty);
                    self.unifier.unify(&Monotype::Integer, &rhs_ty);

                    Monotype::Integer
                }
                ast::BinaryOp::Equal(_)
                | ast::BinaryOp::GreaterThan(_)
                | ast::BinaryOp::LessThan(_) => {
                    let lhs_ty = self.infer_expr(*lhs);
                    let rhs_ty = self.infer_expr(*rhs);
                    self.unifier.unify(&lhs_ty, &rhs_ty);

                    Monotype::Bool
                }
            },
            hir::Expr::Unary { op, expr } => match op {
                ast::UnaryOp::Neg(_) => {
                    let expr_ty = self.infer_expr(*expr);
                    self.unifier.unify(&Monotype::Integer, &expr_ty);

                    Monotype::Integer
                }
                ast::UnaryOp::Not(_) => {
                    let expr_ty = self.infer_expr(*expr);
                    self.unifier.unify(&Monotype::Bool, &expr_ty);

                    Monotype::Bool
                }
            },
            hir::Expr::Block(block) => {
                self.entry_scope();

                for stmt in &block.stmts {
                    self.infer_stmt(stmt);
                }

                let ty = if let Some(tail) = &block.tail {
                    self.infer_expr(*tail)
                } else {
                    // 最後の式がない場合は Unit として扱う
                    Monotype::Unit
                };

                self.exit_scope();

                ty
            }
            hir::Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_ty = self.infer_expr(*condition);
                self.unifier.unify(&Monotype::Bool, &condition_ty);

                let then_ty = self.infer_expr(*then_branch);
                if let Some(else_branch) = else_branch {
                    let else_ty = self.infer_expr(*else_branch);
                    self.unifier.unify(&then_ty, &else_ty);
                } else {
                    // elseブランチがない場合は Unit として扱う
                    self.unifier.unify(&then_ty, &Monotype::Unit);
                }

                then_ty
            }
            hir::Expr::Return { value } => {
                if let Some(return_value) = value {
                    let ty = self.infer_expr(*return_value);
                    self.unifier.unify(&ty, &self.signature.return_type);
                } else {
                    // 何も指定しない場合は Unit を返すものとして扱う
                    self.unifier
                        .unify(&Monotype::Unit, &self.signature.return_type);
                }

                // return自体の戻り値は Never として扱う
                Monotype::Never
            }
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

#[derive(Debug)]
pub struct InferenceResult {
    pub signature_by_function: HashMap<hir::Function, Signature>,
    pub inference_body_result_by_function: HashMap<hir::Function, InferenceBodyResult>,
}

#[derive(Debug)]
pub struct InferenceBodyResult {
    pub type_by_expr: HashMap<hir::ExprId, Monotype>,
    pub errors: Vec<InferenceError>,
}
#[derive(Debug)]
pub enum InferenceError {
    TypeMismatch {
        expected: Monotype,
        actual: Monotype,
    },
}

#[derive(Default)]
pub struct Environment {
    bindings: HashMap<hir::Name, TypeScheme>,
}

#[derive(Default)]
pub struct Context {
    pub gen_counter: u32,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            bindings: HashMap::new(),
        }
    }

    fn free_variables(&self) -> HashSet<u32> {
        let mut union = HashSet::<u32>::new();
        for type_scheme in self.bindings.values() {
            union.extend(type_scheme.free_variables());
        }

        union
    }

    fn with(&self) -> Environment {
        let mut copy = HashMap::<hir::Name, TypeScheme>::new();
        copy.extend(self.bindings.clone());

        Environment { bindings: copy }
    }

    fn generalize(&self, ty: &Monotype) -> TypeScheme {
        TypeScheme {
            variables: ty.free_variables().sub(&self.free_variables()),
            ty: ty.clone(),
        }
    }
}
