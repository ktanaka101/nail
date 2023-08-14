use std::collections;

use la_arena::{Arena, Idx};

pub fn infer(hir_result: &hir::LowerResult) -> InferenceResult {
    let inferencer = TypeInferencer::new(hir_result);
    inferencer.infer()
}

/// 型推論の結果
#[derive(Debug)]
pub struct InferenceResult {
    /// 式に対応する型
    pub type_by_expr: collections::HashMap<hir::ExprId, ResolvedType>,
    /// パラメータに対応する型
    pub type_by_param: collections::HashMap<hir::ParamId, ResolvedType>,
    /// 関数シグネチャ一覧
    pub signatures: Arena<Signature>,
    /// 関数に対応するシグネチャ
    pub signature_by_function: collections::HashMap<hir::FunctionId, Idx<Signature>>,
    /// 型推論中に発生したエラー
    pub errors: Vec<InferenceError>,
}

#[derive(Debug)]
struct InferenceContext {
    type_by_expr: collections::HashMap<hir::ExprId, ResolvedType>,
    type_by_param: collections::HashMap<hir::ParamId, ResolvedType>,
    signatures: Arena<Signature>,
    signature_by_function: collections::HashMap<hir::FunctionId, Idx<Signature>>,
    errors: Vec<InferenceError>,
}

impl InferenceContext {
    fn new() -> Self {
        Self {
            type_by_expr: collections::HashMap::new(),
            type_by_param: collections::HashMap::new(),
            signatures: Arena::new(),
            signature_by_function: collections::HashMap::new(),
            errors: Vec::new(),
        }
    }
}

/// 型推論中に発生したエラー
#[derive(Debug, Clone)]
pub enum InferenceError {}

/// 解決後の型
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolvedType {
    /// 未解決の型
    Unknown,
    /// 数値型
    Integer,
    /// 文字列型
    String,
    /// 文字型
    Char,
    /// 真偽値型
    Bool,
    /// 単一値型
    Unit,
    /// 値を取り得ないことを表す型
    ///
    /// 例えば、必ず`panic`を起こす関数の型は`Never`です。
    Never,
    /// 関数型
    #[allow(dead_code)]
    Function(Idx<Signature>),
}

/// 関数シグネチャ
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    /// パラメータの型一覧
    ///
    /// パラメータの順番に対応しています。
    pub params: Vec<ResolvedType>,
    /// 戻り値の型
    pub return_type: ResolvedType,
}

/// 型推論器
struct TypeInferencer<'a> {
    hir_result: &'a hir::LowerResult,
    ctx: InferenceContext,
}
impl<'a> TypeInferencer<'a> {
    fn new(hir_result: &'a hir::LowerResult) -> Self {
        Self {
            hir_result,
            ctx: InferenceContext::new(),
        }
    }

    fn infer(mut self) -> InferenceResult {
        for (function_id, function) in self.hir_result.db.functions() {
            let mut params = vec![];
            for param in &function.params {
                let ty = self.infer_ty(&param.lookup(&self.hir_result.db).ty);
                params.push(ty);
                self.ctx.type_by_param.insert(*param, ty);
            }

            let signature = Signature {
                params,
                return_type: self.infer_ty(&function.return_type),
            };
            let signature_idx = self.ctx.signatures.alloc(signature);
            self.ctx
                .signature_by_function
                .insert(function_id, signature_idx);
        }

        for (function_id, _) in self.hir_result.db.functions() {
            let body_ast_id = self
                .hir_result
                .item_tree
                .block_id_by_function(&function_id)
                .unwrap();
            let body = self
                .hir_result
                .shared_ctx
                .function_body_by_block(body_ast_id)
                .unwrap();
            match body {
                hir::Expr::Block(block) => self.infer_block(block),
                _ => unreachable!(),
            };
        }

        InferenceResult {
            type_by_expr: self.ctx.type_by_expr,
            type_by_param: self.ctx.type_by_param,
            signatures: self.ctx.signatures,
            signature_by_function: self.ctx.signature_by_function,
            errors: self.ctx.errors,
        }
    }

    fn infer_ty(&self, ty: &hir::Type) -> ResolvedType {
        match ty {
            hir::Type::Unknown => ResolvedType::Unknown,
            hir::Type::Integer => ResolvedType::Integer,
            hir::Type::String => ResolvedType::String,
            hir::Type::Char => ResolvedType::Char,
            hir::Type::Boolean => ResolvedType::Bool,
            hir::Type::Unit => ResolvedType::Unit,
        }
    }

    fn infer_stmts(&mut self, stmts: &[hir::Stmt]) {
        for stmt in stmts {
            match stmt {
                hir::Stmt::ExprStmt { expr, .. } => {
                    let ty = self.infer_expr_id(*expr);
                    self.ctx.type_by_expr.insert(*expr, ty);
                }
                hir::Stmt::VariableDef { value, .. } => {
                    let ty = self.infer_expr_id(*value);
                    self.ctx.type_by_expr.insert(*value, ty);
                }
                hir::Stmt::Item { .. } => (),
            }
        }
    }

    fn infer_expr(&mut self, expr: &hir::Expr) -> ResolvedType {
        match expr {
            hir::Expr::Symbol(symbol) => match symbol {
                hir::Symbol::Local { expr, .. } => self.infer_expr_id(*expr),
                hir::Symbol::Param { param, .. } => {
                    let param = &param.lookup(&self.hir_result.db);
                    self.infer_ty(&param.ty)
                }
                hir::Symbol::Missing { .. } => ResolvedType::Unknown,
                hir::Symbol::Function { .. } => unimplemented!(),
            },
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Integer(_) => ResolvedType::Integer,
                hir::Literal::String(_) => ResolvedType::String,
                hir::Literal::Char(_) => ResolvedType::Char,
                hir::Literal::Bool(_) => ResolvedType::Bool,
            },
            hir::Expr::Binary { op, lhs, rhs } => {
                // TODO: supports string equal
                let lhs_ty = self.infer_expr_id(*lhs);
                let rhs_ty = self.infer_expr_id(*rhs);

                match op {
                    ast::BinaryOp::Add(_)
                    | ast::BinaryOp::Sub(_)
                    | ast::BinaryOp::Mul(_)
                    | ast::BinaryOp::Div(_) => {
                        if rhs_ty == lhs_ty && (matches!(rhs_ty, ResolvedType::Integer)) {
                            return rhs_ty;
                        }
                    }
                    ast::BinaryOp::Equal(_) => {
                        if rhs_ty == lhs_ty
                            && (matches!(rhs_ty, ResolvedType::Integer | ResolvedType::Bool))
                        {
                            return ResolvedType::Bool;
                        }
                    }
                    ast::BinaryOp::GreaterThan(_) | ast::BinaryOp::LessThan(_) => {
                        if rhs_ty == lhs_ty && (matches!(rhs_ty, ResolvedType::Integer)) {
                            return ResolvedType::Bool;
                        }
                    }
                }

                match (lhs_ty, rhs_ty) {
                    (ty, ResolvedType::Unknown) => {
                        self.ctx.type_by_expr.insert(*rhs, ty);
                        ty
                    }
                    (ResolvedType::Unknown, ty) => {
                        self.ctx.type_by_expr.insert(*lhs, ty);
                        ty
                    }
                    (_, _) => ResolvedType::Unknown,
                }
            }
            hir::Expr::Unary { op, expr } => {
                let expr_ty = self.infer_expr_id(*expr);
                match op {
                    ast::UnaryOp::Neg(_) => {
                        if expr_ty == ResolvedType::Integer {
                            return ResolvedType::Integer;
                        }
                    }
                    ast::UnaryOp::Not(_) => {
                        if expr_ty == ResolvedType::Bool {
                            return ResolvedType::Bool;
                        }
                    }
                }

                ResolvedType::Unknown
            }
            hir::Expr::Call { callee, args } => match callee {
                hir::Symbol::Missing { .. } => ResolvedType::Unknown,
                hir::Symbol::Function { function, .. } => {
                    let signature = function.lookup(&self.hir_result.db);

                    for (i, arg) in args.iter().enumerate() {
                        let param = signature.params[i];

                        let arg_ty = self.infer_expr_id(*arg);
                        let param_ty = self.infer_ty(&param.lookup(&self.hir_result.db).ty);

                        if arg_ty == param_ty {
                            continue;
                        }

                        match (arg_ty, param_ty) {
                            (ResolvedType::Unknown, ResolvedType::Unknown) => (),
                            (ResolvedType::Unknown, ty) => {
                                self.ctx.type_by_expr.insert(*arg, ty);
                            }
                            (_, _) => (),
                        }
                    }

                    self.infer_ty(&signature.return_type)
                }
                hir::Symbol::Local { .. } | hir::Symbol::Param { .. } => unimplemented!(),
            },
            hir::Expr::Block(block) => self.infer_block(block),
            hir::Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.infer_expr_id(*condition);
                let then_branch_ty = self.infer_expr_id(*then_branch);
                let else_branch_ty = if let Some(else_branch) = else_branch {
                    self.infer_expr_id(*else_branch)
                } else {
                    ResolvedType::Unit
                };

                match (then_branch_ty, else_branch_ty) {
                    (ResolvedType::Unknown, ResolvedType::Unknown) => ResolvedType::Unknown,
                    (ResolvedType::Unknown, ty) => {
                        self.ctx.type_by_expr.insert(*then_branch, ty);
                        ty
                    }
                    (ty, ResolvedType::Unknown) => {
                        self.ctx.type_by_expr.insert(else_branch.unwrap(), ty);
                        ty
                    }
                    (ty_a, ty_b) if ty_a == ty_b => ty_a,
                    (_, _) => ResolvedType::Unknown,
                }
            }
            hir::Expr::Return { value } => {
                if let Some(value) = value {
                    self.infer_expr_id(*value);
                }
                ResolvedType::Never
            }
            hir::Expr::Missing => ResolvedType::Unknown,
        }
    }

    fn infer_block(&mut self, block: &hir::Block) -> ResolvedType {
        self.infer_stmts(&block.stmts);
        if let Some(tail) = block.tail {
            self.infer_expr_id(tail)
        } else {
            ResolvedType::Unit
        }
    }

    fn infer_expr_id(&mut self, expr_id: hir::ExprId) -> ResolvedType {
        if let Some(ty) = self.lookup_type(expr_id) {
            return ty;
        }

        let ty = self.infer_expr(expr_id.lookup(&self.hir_result.shared_ctx));
        self.ctx.type_by_expr.insert(expr_id, ty);

        ty
    }

    fn lookup_type(&self, expr_id: hir::ExprId) -> Option<ResolvedType> {
        self.ctx.type_by_expr.get(&expr_id).copied()
    }
}
