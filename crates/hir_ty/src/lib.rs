use std::collections;

use hir::BodyLowerContext;

pub fn infer(hir_result: &hir::LowerResult) -> InferenceResult {
    let inferencer = TypeInferencer::new(hir_result);
    inferencer.infer()
}

#[derive(Debug)]
pub struct InferenceResult {
    pub type_by_exprs: collections::HashMap<hir::ExprIdx, ResolvedType>,
    pub errors: Vec<InferenceError>,
}

#[derive(Debug)]
struct InferenceContext {
    pub type_by_exprs: collections::HashMap<hir::ExprIdx, ResolvedType>,
    pub errors: Vec<InferenceError>,
}

impl InferenceContext {
    fn new() -> Self {
        Self {
            type_by_exprs: collections::HashMap::new(),
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
}

struct TypeInferencer {

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

    pub fn infer(mut self) -> InferenceResult {
        self.infer_body(&self.hir_result.stmts, &self.hir_result.root_ctx);

        InferenceResult {
            type_by_exprs: self.ctx.type_by_exprs,
            signatures: self.ctx.signatures,
            errors: self.ctx.errors,
        }
    }

    pub fn infer_body(&mut self, stmts: &[hir::Stmt], lower_ctx: &hir::BodyLowerContext) {
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
                hir::Stmt::FunctionDef { .. } => unimplemented!(),
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
                hir::Symbol::Missing { .. } => ResolvedType::Unknown,
                hir::Symbol::Function { .. } | hir::Symbol::Param { .. } => unimplemented!(),
            },
            hir::Expr::Call { .. } => unimplemented!(),
            hir::Expr::Block(_) => unimplemented!(),
            hir::Expr::Missing => ResolvedType::Unknown,
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

        self.infer_expr(&lower_ctx.exprs[expr], lower_ctx)
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
}
