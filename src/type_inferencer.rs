use std::convert::TryInto;

use anyhow::Result;

use crate::parser::ast;

#[derive(Default)]
pub struct TypeInferencer {}

#[derive(thiserror::Error, Debug, Clone)]
pub enum Error {}

impl TypeInferencer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn inference(&self, node: ast::Node) -> Result<ast::Node> {
        match node {
            ast::Node::Program(mut prg) => {
                prg.statements = prg
                    .statements
                    .into_iter()
                    .map(|stmt| self.inference(stmt.into()).unwrap().try_into().unwrap())
                    .collect::<Vec<_>>();
                Ok(prg.into())
            }
            ast::Node::Stmt(stmt) => match stmt {
                ast::Stmt::Let(mut mlet) => {
                    mlet.name.mtype = self.inference_expr(&mlet.value)?;
                    Ok(ast::Stmt::from(mlet).into())
                }
                other => Ok(other.into()),
            },
            other => Ok(other),
        }
    }

    pub fn inference_expr(&self, expr: &ast::Expr) -> Result<Option<ast::Type>> {
        Ok(Some(match &expr {
            ast::Expr::Array(_) => ast::Type::Array,
            ast::Expr::Boolean(_) => ast::Type::Boolean,
            ast::Expr::Char(_) => ast::Type::Char,
            ast::Expr::Function(_) => ast::Type::Function,
            ast::Expr::Hash(_) => ast::Type::Hash,
            ast::Expr::Integer(_) => ast::Type::Integer,
            ast::Expr::StringLit(_) => ast::Type::String,
            _other => unimplemented!(),
        }))
    }
}

#[cfg(test)]
mod tests {
    use anyhow::{anyhow, Result};

    use super::*;

    #[test]
    fn inference_types() {
        let tests = vec![
            ("[]", Some(ast::Type::Array)),
            ("true", Some(ast::Type::Boolean)),
            ("false", Some(ast::Type::Boolean)),
            ("|| { }", Some(ast::Type::Function)),
            ("fn xxx() {}", Some(ast::Type::Function)),
            ("{ 1: 2 }", Some(ast::Type::Hash)),
            ("1", Some(ast::Type::Integer)),
            ("'a'", Some(ast::Type::Char)),
            (r#""aaa""#, Some(ast::Type::String)),
        ];

        parse_each(tests, |type_inferencer, result_node, input, expected| {
            assert!(
                result_node.statements.len() == 1,
                make_err_msg(input, &expected, &None)
            );
            match &result_node.statements[0] {
                ast::Stmt::ExprStmt(expr_stmt) => {
                    assert_eq!(
                        &type_inferencer
                            .inference_expr(&expr_stmt.expr)
                            .unwrap_or_else(|e| panic_err(input, &expected, &Some(e))),
                        expected
                    );
                }
                other => panic_err(
                    input,
                    &expected,
                    &Some(anyhow!("expected ExprStmt. actual: {}", other)),
                ),
            }

            Ok(())
        });
    }

    #[test]
    fn inference_let_type() {
        let tests = vec![
            ("let a = [1, 2, 3]", Some(ast::Type::Array)),
            ("let a = true", Some(ast::Type::Boolean)),
            ("let a = false", Some(ast::Type::Boolean)),
            ("let a = || { }", Some(ast::Type::Function)),
            ("let a = { 1: 2 }", Some(ast::Type::Hash)),
            ("let a = 1", Some(ast::Type::Integer)),
            ("let a = 'a'", Some(ast::Type::Char)),
            (r#"let a = "aaa""#, Some(ast::Type::String)),
        ];

        parse_each(tests, |type_inferencer, program, input, expected| {
            let result_node: ast::Program =
                type_inferencer.inference(program.into())?.try_into()?;

            assert!(
                result_node.statements.len() == 1,
                make_err_msg(input, &expected, &None)
            );
            match &result_node.statements[0] {
                ast::Stmt::Let(mlet) => {
                    assert_eq!(
                        &mlet.name.mtype,
                        expected,
                        "{}",
                        make_err_msg(input, &expected, &None)
                    );
                }
                other => panic_err(
                    input,
                    &expected,
                    &Some(anyhow!("expected ExprStmt. actual: {}", other)),
                ),
            }

            Ok(())
        });
    }

    fn parse_each<F>(tests: Vec<(&'static str, Option<ast::Type>)>, f: F)
    where
        F: Fn(TypeInferencer, ast::Program, &'static str, &Option<ast::Type>) -> Result<()>,
    {
        tests.into_iter().for_each(|(input, r#type)| {
            let program = run_parse(input).unwrap_or_else(|e| panic_err(input, &r#type, &Some(e)));
            let type_inferencer = TypeInferencer::new();
            f(type_inferencer, program, &input, &r#type)
                .unwrap_or_else(|e| panic_err(input, &r#type, &Some(e)));
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
