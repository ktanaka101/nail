use std::{collections::HashMap, convert::TryInto};

use anyhow::{anyhow, Result};

use crate::parser::ast;

#[derive(Default)]
pub struct TypeInferencer {
    type_table: HashMap<String, Option<ast::Type>>,
}

#[derive(thiserror::Error, Debug, Clone)]
pub enum Error {}

impl TypeInferencer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn infer(&mut self, node: ast::Node) -> Result<ast::Node> {
        match node {
            ast::Node::Program(mut prg) => {
                prg.statements = prg
                    .statements
                    .into_iter()
                    .map(|stmt| self.infer(stmt.into()).unwrap().try_into().unwrap())
                    .collect::<Vec<_>>();
                Ok(prg.into())
            }
            ast::Node::Stmt(stmt) => match stmt {
                ast::Stmt::Let(mut mlet) => {
                    mlet.name.mtype = self.infer_expr(&mlet.value)?;
                    self.type_table
                        .insert(mlet.name.value.clone(), mlet.name.mtype.clone());
                    Ok(ast::Stmt::from(mlet).into())
                }
                ast::Stmt::ExprStmt(mut expr_stmt) => {
                    match expr_stmt.expr {
                        ast::Expr::Identifier(ref mut id) => {
                            id.mtype = self.infer_identifier(id)?;
                        }
                        ast::Expr::If(ref mut r#if) => {
                            let ty = self.infer_if(r#if)?;
                            r#if.r#type = ty;
                        }
                        _other => unimplemented!(),
                    };
                    Ok(ast::Stmt::from(expr_stmt).into())
                }
                other => Ok(other.into()),
            },
            ast::Node::Expr(mut expr) => {
                match expr {
                    ast::Expr::If(ref mut r#if) => {
                        let ty = self.infer_if(r#if)?;
                        r#if.r#type = ty;
                    }
                    _other => unimplemented!(),
                }
                Ok(expr.into())
            }
        }
    }

    pub fn infer_identifier(&self, id: &ast::Identifier) -> Result<Option<ast::Type>> {
        let ty = self
            .type_table
            .get(&id.value)
            .ok_or_else(|| anyhow!("Undefined identifier by {}", id.value))?;
        Ok(ty.to_owned())
    }

    pub fn infer_expr(&self, expr: &ast::Expr) -> Result<Option<ast::Type>> {
        Ok(Some(match &expr {
            ast::Expr::Array(_) => ast::Type::Array,
            ast::Expr::Boolean(_) => ast::Type::Boolean,
            ast::Expr::Char(_) => ast::Type::Char,
            ast::Expr::Function(_) => ast::Type::Function,
            ast::Expr::Hash(_) => ast::Type::Hash,
            ast::Expr::Integer(_) => ast::Type::Integer,
            ast::Expr::StringLit(_) => ast::Type::String,
            ast::Expr::Identifier(id) => return self.infer_identifier(id),
            ast::Expr::If(r#if) => return self.infer_if(r#if),
            _other => unimplemented!(),
        }))
    }

    fn infer_stmt(&self, stmt: &ast::Stmt) -> Result<Option<ast::Type>> {
        Ok(match stmt {
            ast::Stmt::Let(_) => Some(ast::Type::Never),
            ast::Stmt::Return(_) => Some(ast::Type::Never),
            ast::Stmt::Block(b) => {
                let stmt = &b.last_stmt();
                if let Some(stmt) = stmt {
                    self.infer_stmt(stmt)?
                } else {
                    None
                }
            }
            ast::Stmt::ExprStmt(es) => self.infer_expr(&es.expr)?,
        })
    }

    pub fn infer_block(&self, block: &ast::Block) -> Result<Option<ast::Type>> {
        let stmt = block.last_stmt();

        if let Some(ref stmt) = stmt {
            self.infer_stmt(stmt)
        } else {
            Ok(None)
        }
    }

    pub fn infer_if(&self, r#if: &ast::If) -> Result<Option<ast::Type>> {
        let consequence_type = self.infer_block(&r#if.consequence)?;

        if let Some(alternative) = &r#if.alternative {
            Ok(Some(ast::Type::new_union(vec![
                consequence_type.unwrap(),
                self.infer_block(alternative).unwrap().unwrap(),
            ])))
        } else {
            Ok(consequence_type)
        }
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
                            .infer_expr(&expr_stmt.expr)
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
            (
                r#"
                    let a = if true {
                        10
                    } else {
                        15
                    }
                "#,
                Some(ast::Type::new_union(vec![ast::Type::Integer])),
            ),
            (
                r#"
                    let a = if true {
                        10
                    } else {
                        "xxx"
                    }
                "#,
                Some(ast::Type::new_union(vec![
                    ast::Type::Integer,
                    ast::Type::String,
                ])),
            ),
            (
                r#"
                    let a = if true {
                        10
                    }
                "#,
                Some(ast::Type::Integer),
            ),
        ];

        parse_each(tests, |mut type_inferencer, program, input, expected| {
            let result_node: ast::Program = type_inferencer.infer(program.into())?.try_into()?;

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

    #[test]
    fn inference_identifier_type() {
        let tests = vec![
            ("let a = [1, 2, 3]\n a", Some(ast::Type::Array)),
            ("let a = true\n a", Some(ast::Type::Boolean)),
            ("let a = false\n a", Some(ast::Type::Boolean)),
            ("let a = || { }\n a", Some(ast::Type::Function)),
            ("let a = { 1: 2 }\n a", Some(ast::Type::Hash)),
            ("let a = 1\n a", Some(ast::Type::Integer)),
            ("let a = 'a'\n a", Some(ast::Type::Char)),
            ("let a = \"aaa\"\n a", Some(ast::Type::String)),
            (
                r#"
                    let a = if true {
                        10
                    } else {
                        15
                    }
                    a
                "#,
                Some(ast::Type::new_union(vec![ast::Type::Integer])),
            ),
            (
                r#"
                    let a = if true {
                        10
                    } else {
                        "xxx"
                    }
                    a
                "#,
                Some(ast::Type::new_union(vec![
                    ast::Type::Integer,
                    ast::Type::String,
                ])),
            ),
            (
                r#"
                    let a = if true {
                        10
                    }
                    a
                "#,
                Some(ast::Type::Integer),
            ),
        ];

        parse_each(tests, |mut type_inferencer, program, input, expected| {
            let result_node: ast::Program = type_inferencer.infer(program.into())?.try_into()?;

            assert!(
                result_node.statements.len() == 2,
                make_err_msg(input, &expected, &None)
            );
            match &result_node.statements[1] {
                ast::Stmt::ExprStmt(expr_stmt) => match &expr_stmt.expr {
                    ast::Expr::Identifier(id) => {
                        assert_eq!(
                            &id.mtype,
                            expected,
                            "{}",
                            make_err_msg(input, &expected, &None)
                        );
                    }
                    other => panic_err(
                        input,
                        &expected,
                        &Some(anyhow!("expected Identifier. actual: {}", other)),
                    ),
                },
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
    fn inference_if_type() {
        let tests = vec![
            (
                r#"
                    if true {
                        10
                    }
                "#,
                Some(ast::Type::Integer),
            ),
            (
                r#"
                    if true {
                        10
                    } else {
                        "xxx"
                    }
                "#,
                Some(ast::Type::new_union(vec![
                    ast::Type::Integer,
                    ast::Type::String,
                ])),
            ),
            (
                r#"
                    if true {
                        10
                    } else {
                        15
                    }
                "#,
                Some(ast::Type::Integer),
            ),
        ];

        parse_each(tests, |mut type_inferencer, program, input, expected| {
            let result_node: ast::Program = type_inferencer.infer(program.into())?.try_into()?;

            assert!(
                result_node.statements.len() == 1,
                make_err_msg(input, &expected, &None)
            );
            match &result_node.statements[0] {
                ast::Stmt::ExprStmt(expr_stmt) => match &expr_stmt.expr {
                    ast::Expr::If(r#if) => {
                        assert_eq!(
                            &r#if.r#type,
                            expected,
                            "{}",
                            format!("input: {}\nexpected: {:?}", input, expected)
                        );
                    }
                    other => panic_err(
                        input,
                        &expected,
                        &Some(anyhow!("expected If. actual: {}", other)),
                    ),
                },
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
