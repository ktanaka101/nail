mod type_resolver;

use anyhow::Result;
use type_resolver::TypeResolver;

use crate::parser::ast;

#[derive(thiserror::Error, Debug, Clone)]
pub enum Error {}

#[derive(Default)]
pub struct Normalizer {}

impl Normalizer {
    pub fn new() -> Self {
        Normalizer {}
    }

    pub fn normalize(&self, node: ast::Node) -> Result<ast::Node> {
        Ok(node.clone())
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
