use std::collections;

pub fn infer(stmts: Vec<hir::Stmt>, lower_ctx: &hir::BodyLowerContext) -> InferenceResult {
    let inferencer = TypeInferencer::new();
    inferencer.infer(stmts, lower_ctx)
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
pub enum InferenceError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolvedType {
    Unknown,
    Integer,
    String,
    Char,
    Bool,
}

struct TypeInferencer {
    ctx: InferenceContext,
}
impl TypeInferencer {
    fn new() -> Self {
        Self {
            ctx: InferenceContext::new(),
        }
    }

    pub fn infer(
        mut self,
        stmts: Vec<hir::Stmt>,
        lower_ctx: &hir::BodyLowerContext,
    ) -> InferenceResult {
        self.infer_body(stmts, lower_ctx);

        InferenceResult {
            type_by_exprs: self.ctx.type_by_exprs,
            errors: self.ctx.errors,
        }
    }

    pub fn infer_body(&mut self, stmts: Vec<hir::Stmt>, ctx: &hir::BodyLowerContext) {
        for stmt in stmts {
            match stmt {
                hir::Stmt::Expr(expr) => {
                    let ty = self.infer_expr(&ctx.exprs[expr]);
                    self.ctx.type_by_exprs.insert(expr, ty);
                }
                hir::Stmt::VariableDef { value, .. } => {
                    let ty = self.infer_expr(&ctx.exprs[value]);
                    self.ctx.type_by_exprs.insert(value, ty);
                }
                hir::Stmt::FunctionDef { .. } => unimplemented!(),
            }
        }
    }

    fn infer_expr(&mut self, expr: &hir::Expr) -> ResolvedType {
        match expr {
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Integer(_) => ResolvedType::Integer,
                hir::Literal::String(_) => ResolvedType::String,
                hir::Literal::Char(_) => ResolvedType::Char,
                hir::Literal::Bool(_) => ResolvedType::Bool,
            },
            _ => todo!(),
        }
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
        let inferencer = TypeInferencer::new();
        let result = inferencer.infer(lower_result.stmts, &lower_result.root_ctx);
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
            "#]],
        );
    }

    #[test]
    fn infer_string_literal() {
        check(
            "\"aaa\"",
            expect![[r#"
                0: string
            "#]],
        );
    }

    #[test]
    fn infer_char_literal() {
        check(
            "'a'",
            expect![[r#"
                0: char
            "#]],
        );
    }

    #[test]
    fn infer_bool_test() {
        check(
            "true",
            expect![[r#"
                0: bool
            "#]],
        );

        check(
            "false",
            expect![[r#"
                0: bool
            "#]],
        );
    }

    #[test]
    fn infer_variable_def() {
        check(
            "let a = true",
            expect![[r#"
                0: bool
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
            "#]],
        )
    }
}
