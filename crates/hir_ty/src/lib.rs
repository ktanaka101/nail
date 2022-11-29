use std::collections;

#[derive(Debug)]
pub struct InferenceResult {
    pub type_by_exprs: collections::HashMap<hir::ExprIdx, ResolvedType>,
    pub errors: Vec<InferenceError>,
}

impl InferenceResult {
    fn new() -> Self {
        Self {
            type_by_exprs: collections::HashMap::new(),
            errors: Vec::new(),
        }
    }

    #[cfg(test)]
    fn debug(&self) -> String {
        let mut msg = "".to_string();
        let mut indexes = self.type_by_exprs.keys().collect::<Vec<_>>();
        indexes.sort_by_cached_key(|idx| idx.into_raw());
        for idx in indexes {
            msg.push_str(&format!(
                "{:?}: {:?}\n",
                idx.into_raw(),
                self.type_by_exprs[idx]
            ));
        }

        msg
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

pub fn infer_body(stmts: Vec<hir::Stmt>, ctx: &hir::BodyLowerContext) -> InferenceResult {
    let mut inference_result = InferenceResult::new();

    for stmt in stmts {
        match stmt {
            hir::Stmt::Expr(expr) => {
                let ty = infer_expr(&ctx.exprs[expr]);
                inference_result.type_by_exprs.insert(expr, ty);
            }
            hir::Stmt::VariableDef { value, .. } => {
                let ty = infer_expr(&ctx.exprs[value]);
                inference_result.type_by_exprs.insert(value, ty);
            }
            hir::Stmt::FunctionDef { .. } => unimplemented!(),
        }
    }

    inference_result
}

fn infer_expr(expr: &hir::Expr) -> ResolvedType {
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

#[cfg(test)]
mod tests {
    use super::*;
    use ast::AstNode;
    use expect_test::{expect, Expect};

    fn check(input: &str, expect: Expect) {
        let parsed = parser::parse(input);
        let ast = ast::SourceFile::cast(parsed.syntax()).unwrap();
        let (_root_ctx, ctx, body, _db, _item_tree, _interner) = hir::lower(ast);
        let result = infer_body(body, &ctx);
        expect.assert_eq(&result.debug());
    }

    #[test]
    fn infer_integer_literal() {
        check(
            "10",
            expect![[r#"
                0: Integer
            "#]],
        );
    }

    #[test]
    fn infer_string_literal() {
        check(
            "\"aaa\"",
            expect![[r#"
                0: String
            "#]],
        );
    }

    #[test]
    fn infer_char_literal() {
        check(
            "'a'",
            expect![[r#"
                0: Char
            "#]],
        );
    }

    #[test]
    fn infer_bool_test() {
        check(
            "true",
            expect![[r#"
                0: Bool
            "#]],
        );

        check(
            "false",
            expect![[r#"
                0: Bool
            "#]],
        );
    }

    #[test]
    fn infer_variable_def() {
        check(
            "let a = true",
            expect![[r#"
                0: Bool
            "#]],
        )
    }

    #[test]
    fn infer_multiline_variable_def() {
        check(
            r#"
                let a = true
                let b = false
            "#,
            expect![[r#"
                0: Bool
                1: Bool
            "#]],
        )
    }
}
