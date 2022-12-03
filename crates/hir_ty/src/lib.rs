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
pub enum InferenceError {
    UnresolvedType(hir::ExprIdx),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolvedType {
    Unknown,
    Integer,
    String,
    Char,
    Bool,
    Unit,
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
                hir::Expr::Block(block) => {
                    self.infer_stmts(&block.stmts, body_ctx);
                }
                _ => unreachable!(),
            }
        }

        self.infer_stmts(&self.hir_result.stmts, &self.hir_result.root_ctx);

        InferenceResult {
            type_by_exprs: self.ctx.type_by_exprs,
            signatures: self.ctx.signatures,
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
                    (ResolvedType::Unknown, ResolvedType::Unknown) => {
                        self.ctx.errors.push(InferenceError::UnresolvedType(*lhs));
                        self.ctx.errors.push(InferenceError::UnresolvedType(*rhs));
                        ResolvedType::Unknown
                    }
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
            hir::Expr::Call { .. } => unimplemented!(),
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

        self.infer_expr(&self.hir_result.shared_ctx.exprs[expr], lower_ctx)
    }

    fn lookup_type(&self, expr: hir::ExprIdx) -> Option<ResolvedType> {
        self.ctx.type_by_exprs.get(&expr).copied()
    }
}

#[cfg(test)]
mod tests {
    use ast::AstNode;
    use expect_test::{expect, Expect};

    use super::*;

    fn check(input: &str, expect: Expect) {
        let parsed = parser::parse(input);
        let ast = ast::SourceFile::cast(parsed.syntax()).unwrap();
        let lower_result = hir::lower(ast);
        let inferencer = TypeInferencer::new(&lower_result);
        let result = inferencer.infer();
        expect.assert_eq(&debug(&result));
    }

    fn debug(result: &InferenceResult) -> String {
        let mut msg = "".to_string();
        let mut indexes = result.type_by_exprs.keys().collect::<Vec<_>>();
        indexes.sort_by_cached_key(|idx| idx.into_raw());
        for idx in indexes {
            msg.push_str(&format!(
                "{}: {}\n",
                idx.into_raw(),
                debug_type(&result.type_by_exprs[idx])
            ));
        }

        msg.push_str("---\n");
        for err in &result.errors {
            match err {
                InferenceError::UnresolvedType(idx) => {
                    msg.push_str(&format!(
                        "error: {} is not resolved type.\n",
                        idx.into_raw()
                    ));
                }
            }
        }

        msg
    }

    fn debug_type(ty: &ResolvedType) -> String {
        match ty {
            ResolvedType::Unknown => "unknown",
            ResolvedType::Integer => "int",
            ResolvedType::String => "string",
            ResolvedType::Char => "char",
            ResolvedType::Bool => "bool",
            ResolvedType::Unit => "()",
            ResolvedType::Function(_) => "fn",
        }
        .to_string()
    }

    #[test]
    fn infer_integer_literal() {
        check(
            "10",
            expect![[r#"
                0: int
                ---
            "#]],
        );
    }

    #[test]
    fn infer_string_literal() {
        check(
            "\"aaa\"",
            expect![[r#"
                0: string
                ---
            "#]],
        );
    }

    #[test]
    fn infer_char_literal() {
        check(
            "'a'",
            expect![[r#"
                0: char
                ---
            "#]],
        );
    }

    #[test]
    fn infer_bool_test() {
        check(
            "true",
            expect![[r#"
                0: bool
                ---
            "#]],
        );

        check(
            "false",
            expect![[r#"
                0: bool
                ---
            "#]],
        );
    }

    #[test]
    fn infer_variable_def() {
        check(
            "let a = true",
            expect![[r#"
                0: bool
                ---
            "#]],
        )
    }

    #[test]
    fn infer_multiline_variable_def() {
        check(
            r#"
                let a = true
                let b = 10
                let c = "aa"
                let d = 'a'
            "#,
            expect![[r#"
                0: bool
                1: int
                2: string
                3: char
                ---
            "#]],
        )
    }

    #[test]
    fn infer_binary() {
        check(
            r#"
                10 + 20
                "aaa" + "bbb"
                10 + "aaa"
                'a' + 'a'
                10 + 'a'
                true + true
                10 + true
                10 + (10 + "aaa")
            "#,
            expect![[r#"
                2: int
                5: unknown
                8: unknown
                11: unknown
                14: unknown
                17: unknown
                20: unknown
                24: int
                25: int
                ---
            "#]],
        );

        check(
            r#"
                (10 + "aaa") + (10 + "aaa")
            "#,
            expect![[r#"
                6: unknown
                ---
                error: 4 is not resolved type.
                error: 5 is not resolved type.
            "#]],
        );
    }

    #[test]
    fn infer_unary() {
        check(
            r#"
                let a = -10
                let b = -"aaa"
                let c = -'a'
                let d = -true
            "#,
            expect![[r#"
                1: int
                3: unknown
                5: unknown
                7: unknown
                ---
            "#]],
        )
    }

    #[test]
    fn infer_variable_ref() {
        check(
            r#"
                let a = -10
                a
            "#,
            expect![[r#"
                1: int
                2: int
                ---
            "#]],
        )
    }

    #[test]
    fn infer_block() {
        check(
            r#"
                {
                    10
                }
            "#,
            expect![[r#"
                1: int
                ---
            "#]],
        );

        check(
            r#"
                {
                    {
                        10
                        "aaa"
                    }
                }
            "#,
            expect![[r#"
                0: int
                3: string
                ---
            "#]],
        );

        check(
            r#"
                let a = 10;
                let b = {
                    let c = 20;
                    a + c
                }
                b
            "#,
            expect![[r#"
                0: int
                1: int
                5: int
                6: int
                ---
            "#]],
        );
    }

    #[test]
    fn infer_function() {
        check(
            r#"
                fn aaa() {
                    let a = 10
                    a
                }
            "#,
            expect![[r#"
                0: int
                ---
            "#]],
        );
    }

    #[test]
    fn infer_function_param() {
        check(
            r#"
                fn aaa(x: int, y: string) {
                    let a = x
                    let b = y
                }
            "#,
            expect![[r#"
                0: int
                1: string
                ---
            "#]],
        );
    }
}
