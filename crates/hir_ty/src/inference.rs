use std::collections;

use hir::BodyLowerContext;
use la_arena::{Arena, Idx};

pub fn infer(hir_result: &hir::LowerResult) -> InferenceResult {
    let inferencer = TypeInferencer::new(hir_result);
    inferencer.infer()
}

#[derive(Debug)]
pub struct InferenceResult {
    pub type_by_exprs: collections::HashMap<hir::ExprIdx, ResolvedType>,
    pub signatures: Arena<Signature>,
    pub signature_by_function: collections::HashMap<hir::FunctionIdx, Idx<Signature>>,
    pub errors: Vec<InferenceError>,
}

#[derive(Debug)]
struct InferenceContext {
    pub type_by_exprs: collections::HashMap<hir::ExprIdx, ResolvedType>,
    pub signatures: Arena<Signature>,
    pub signature_by_function: collections::HashMap<hir::FunctionIdx, Idx<Signature>>,
    pub errors: Vec<InferenceError>,
}

impl InferenceContext {
    fn new() -> Self {
        Self {
            type_by_exprs: collections::HashMap::new(),
            signatures: Arena::new(),
            signature_by_function: collections::HashMap::new(),
            errors: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum InferenceError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolvedType {
    Unknown,
    Integer,
    String,
    Char,
    Bool,
    Unit,
    #[allow(dead_code)]
    Function(Idx<Signature>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub params: Vec<ResolvedType>,
    pub return_type: ResolvedType,
}

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
        for (function_idx, function) in self.hir_result.db.functions.iter() {
            let signature = Signature {
                params: function
                    .params
                    .iter()
                    .map(|param| self.infer_ty(&self.hir_result.db.params[*param].ty))
                    .collect(),
                return_type: self.infer_ty(&function.return_type),
            };
            let signature_idx = self.ctx.signatures.alloc(signature);
            self.ctx
                .signature_by_function
                .insert(function_idx, signature_idx);
        }

        for (function_idx, _) in self.hir_result.db.functions.iter() {
            let body_ast_id = &self
                .hir_result
                .item_tree
                .function_to_block(&function_idx)
                .unwrap();
            let body_ctx = self
                .hir_result
                .shared_ctx
                .body_context_by_block(body_ast_id)
                .unwrap();
            let body = self
                .hir_result
                .shared_ctx
                .function_body_by_block(body_ast_id)
                .unwrap();
            match body {
                hir::Expr::Block(block) => self.infer_block(block, body_ctx),
                _ => unreachable!(),
            };
        }

        self.infer_stmts(&self.hir_result.stmts, &self.hir_result.root_ctx);

        InferenceResult {
            type_by_exprs: self.ctx.type_by_exprs,
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

    fn infer_stmts(&mut self, stmts: &[hir::Stmt], lower_ctx: &hir::BodyLowerContext) {
        for stmt in stmts {
            match stmt {
                hir::Stmt::Expr(expr) => {
                    let ty = self.infer_expr_idx(*expr, lower_ctx);
                    self.ctx.type_by_exprs.insert(*expr, ty);
                }
                hir::Stmt::VariableDef { value, .. } => {
                    let ty = self.infer_expr_idx(*value, lower_ctx);
                    self.ctx.type_by_exprs.insert(*value, ty);
                }
                hir::Stmt::FunctionDef { .. } => (),
            }
        }
    }

    fn infer_expr(&mut self, expr: &hir::Expr, lower_ctx: &BodyLowerContext) -> ResolvedType {
        match expr {
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Integer(_) => ResolvedType::Integer,
                hir::Literal::String(_) => ResolvedType::String,
                hir::Literal::Char(_) => ResolvedType::Char,
                hir::Literal::Bool(_) => ResolvedType::Bool,
            },
            hir::Expr::Binary { lhs, rhs, .. } => {
                let lhs_ty = self.infer_expr_idx(*lhs, lower_ctx);
                let rhs_ty = self.infer_expr_idx(*rhs, lower_ctx);

                if rhs_ty == lhs_ty && rhs_ty == ResolvedType::Integer {
                    return rhs_ty;
                }

                match (lhs_ty, rhs_ty) {
                    (ty, ResolvedType::Unknown) => {
                        self.ctx.type_by_exprs.insert(*rhs, ty);
                        ty
                    }
                    (ResolvedType::Unknown, ty) => {
                        self.ctx.type_by_exprs.insert(*lhs, ty);
                        ty
                    }
                    (_, _) => ResolvedType::Unknown,
                }
            }
            hir::Expr::Unary { expr, .. } => {
                let expr_ty = self.infer_expr_idx(*expr, lower_ctx);
                if expr_ty == ResolvedType::Integer {
                    return ResolvedType::Integer;
                }

                ResolvedType::Unknown
            }
            hir::Expr::VariableRef { var } => match var {
                hir::Symbol::Local { expr, .. } => self.infer_expr_idx(*expr, lower_ctx),
                hir::Symbol::Param { param, .. } => {
                    let param = &self.hir_result.db.params[*param];
                    self.infer_ty(&param.ty)
                }
                hir::Symbol::Missing { .. } => ResolvedType::Unknown,
                hir::Symbol::Function { .. } => unimplemented!(),
            },
            hir::Expr::Call { callee, args } => match callee {
                hir::Symbol::Missing { .. } => ResolvedType::Unknown,
                hir::Symbol::Function { function, .. } => {
                    let signature = &self.hir_result.db.functions[*function];

                    for (i, arg) in args.iter().enumerate() {
                        let param = signature.params[i];

                        let arg_ty = self.infer_expr_idx(*arg, lower_ctx);
                        let param_ty = self.infer_ty(&self.hir_result.db.params[param].ty);

                        if arg_ty == param_ty {
                            continue;
                        }

                        match (arg_ty, param_ty) {
                            (ResolvedType::Unknown, ResolvedType::Unknown) => (),
                            (ResolvedType::Unknown, ty) => {
                                self.ctx.type_by_exprs.insert(*arg, ty);
                            }
                            (_, _) => (),
                        }
                    }

                    self.infer_ty(&signature.return_type)
                }
                hir::Symbol::Local { .. } | hir::Symbol::Param { .. } => unimplemented!(),
            },
            hir::Expr::Block(block) => self.infer_block(block, lower_ctx),
            hir::Expr::Missing => ResolvedType::Unknown,
        }
    }

    fn infer_block(&mut self, block: &hir::Block, lower_ctx: &BodyLowerContext) -> ResolvedType {
        self.infer_stmts(&block.stmts, lower_ctx);
        if let Some(tail) = block.tail {
            self.infer_expr_idx(tail, lower_ctx)
        } else {
            ResolvedType::Unit
        }
    }

    fn infer_expr_idx(
        &mut self,
        expr: hir::ExprIdx,
        lower_ctx: &hir::BodyLowerContext,
    ) -> ResolvedType {
        if let Some(ty) = self.lookup_type(expr) {
            return ty;
        }

        let ty = self.infer_expr(&self.hir_result.shared_ctx.exprs[expr], lower_ctx);
        self.ctx.type_by_exprs.insert(expr, ty);

        ty
    }

    fn lookup_type(&self, expr: hir::ExprIdx) -> Option<ResolvedType> {
        self.ctx.type_by_exprs.get(&expr).copied()
    }
}
