use std::collections;

#[derive(Debug)]
pub struct InferenceResult {
    pub mapping: collections::HashMap<hir::ExprIdx, ResolvedType>,
    pub errors: Vec<InferenceError>,
}

impl InferenceResult {
    fn new() -> Self {
        Self {
            mapping: collections::HashMap::new(),
            errors: Vec::new(),
        }
    }

    #[cfg(test)]
    fn debug(&self) -> String {
        let mut msg = "".to_string();
        let mut indexes = self.mapping.keys().collect::<Vec<_>>();
        indexes.sort_by_cached_key(|idx| idx.into_raw());
        for idx in indexes {
            msg.push_str(&format!("{:?}: {:?}\n", idx.into_raw(), self.mapping[idx]));
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

pub fn infer_body(body: Vec<hir::Stmt>, database: &hir::Database) -> InferenceResult {
    let mut inference_result = InferenceResult::new();

    for stmt in body {
        match stmt {
            hir::Stmt::Expr(expr) => {
                let ty = infer_expr(&database.exprs[expr]);
                inference_result.mapping.insert(expr, ty);
            }
            hir::Stmt::VariableDef { value, .. } => {
                let ty = infer_expr(&database.exprs[value]);
                inference_result.mapping.insert(value, ty);
            }
        }
    }

    inference_result
}

fn infer_expr(expr: &hir::Expr) -> ResolvedType {
    match expr {
        hir::Expr::Literal(literal) => match literal {
            hir::Literal::Bool(_) => ResolvedType::Bool,
            _ => todo!(),
        },
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    fn check(input: &str, expect: Expect) {
        let parsed = parser::parse(input);
        let ast = ast::SourceFile::cast(parsed.syntax()).unwrap();
        let (db, body) = hir::lower(ast);
        let result = infer_body(body, &db);
        expect.assert_eq(&result.debug());
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