use crate::parser::ast;

pub trait TypeResolver {
    fn resolve_type(&self) -> Option<ast::Type>;
}

impl TypeResolver for ast::Expr {
    fn resolve_type(&self) -> Option<ast::Type> {
        match self {
            ast::Expr::Array(arr) => arr.resolve_type(),
            ast::Expr::Boolean(b) => b.resolve_type(),
            ast::Expr::Function(f) => f.resolve_type(),
            ast::Expr::Hash(hs) => hs.resolve_type(),
            ast::Expr::Integer(int) => int.resolve_type(),
            ast::Expr::Char(c) => c.resolve_type(),
            ast::Expr::StringLit(s) => s.resolve_type(),
            _need_resolve_expr => unimplemented!(),
        }
    }
}

impl TypeResolver for ast::Array {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::Array)
    }
}

impl TypeResolver for ast::Boolean {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::Boolean)
    }
}

impl TypeResolver for ast::Function {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::Function)
    }
}

impl TypeResolver for ast::Char {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::Char)
    }
}

impl TypeResolver for ast::Hash {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::Hash)
    }
}

impl TypeResolver for ast::Integer {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::Integer)
    }
}

impl TypeResolver for ast::StringLit {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::String)
    }
}

#[cfg(test)]
mod tests {
    use std::convert::TryInto;

    use anyhow::{anyhow, Result};

    use super::*;
    use crate::normalizer;

    #[test]
    fn resolve_value_types() {
        let tests = vec![
            ("[1, 2, 3]", Some(ast::Type::Array)),
            ("true", Some(ast::Type::Boolean)),
            ("false", Some(ast::Type::Boolean)),
            ("|| { }", Some(ast::Type::Function)),
            ("fn xxx() {}", Some(ast::Type::Function)),
            ("{ 1: 2 }", Some(ast::Type::Hash)),
            ("1", Some(ast::Type::Integer)),
            ("'a'", Some(ast::Type::Char)),
            (r#""aaa""#, Some(ast::Type::String)),
        ];

        parse_and_normalize_each(tests, |result_node, input, expected| {
            assert!(
                result_node.statements.len() == 1,
                make_err_msg(input, &expected, &None)
            );
            match &result_node.statements[0] {
                ast::Stmt::ExprStmt(expr_stmt) => {
                    assert_eq!(expr_stmt.expr.resolve_type(), expected);
                }
                other => panic_err(
                    input,
                    &expected,
                    &Some(anyhow!("expected ExprStmt. actual: {}", other)),
                ),
            }
        });
    }

    fn parse_and_normalize_each<F>(tests: Vec<(&'static str, Option<ast::Type>)>, f: F)
    where
        F: Fn(ast::Program, &'static str, Option<ast::Type>),
    {
        tests.into_iter().for_each(|(input, r#type)| {
            let program = run_parse(input).unwrap_or_else(|e| panic_err(input, &r#type, &Some(e)));
            let normalizer = normalizer::Normalizer {};
            let node = normalizer
                .normalize(program.into())
                .unwrap_or_else(|e| panic_err(input, &r#type, &Some(e)));
            let program: ast::Program = node
                .try_into()
                .unwrap_or_else(|e| panic_err(input, &r#type, &Some(e)));
            f(program, &input, r#type);
        });
    }

    fn panic_err(
        input: &'static str,
        expected: &Option<ast::Type>,
        err: &Option<anyhow::Error>,
    ) -> ! {
        panic!(make_err_msg(input, expected, err))
    }

    fn make_err_msg(
        input: &'static str,
        expected: &Option<ast::Type>,
        err: &Option<anyhow::Error>,
    ) -> String {
        match err {
            Some(err) => {
                format!("input: {}\nexpected: {:?}\nerror: {}", input, expected, err)
            }
            None => {
                format!("input: {}\nexpected: {:?}", input, expected)
            }
        }
    }

    fn run_parse(input: &str) -> Result<ast::Program> {
        let lexer = crate::lexer::Lexer::new(input.to_string());
        let mut parser = crate::parser::Parser::new(lexer);
        parser.parse_program()
    }
}
