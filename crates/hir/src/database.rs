use syntax::SyntaxKind;

use crate::{BinaryOp, Expr, Literal, Stmt, UnaryOp};
use la_arena::Arena;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Database {
    pub exprs: Arena<Expr>,
}

impl Database {
    pub(super) fn lower_stmt(&mut self, ast: ast::Stmt) -> Option<Stmt> {
        let result = match ast {
            ast::Stmt::VariableDef(ast) => {
                let expr = self.lower_expr(ast.value());
                Stmt::VariableDef {
                    name: ast.name()?.text().into(),
                    value: self.exprs.alloc(expr),
                }
            }
            ast::Stmt::Expr(ast) => {
                let expr = self.lower_expr(Some(ast));
                Stmt::Expr(self.exprs.alloc(expr))
            }
        };

        Some(result)
    }

    fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Expr {
        if let Some(ast) = ast {
            match ast {
                ast::Expr::BinaryExpr(ast) => self.lower_binary(ast),
                ast::Expr::Literal(ast) => self.lower_literal(ast),
                ast::Expr::ParenExpr(ast) => self.lower_expr(ast.expr()),
                ast::Expr::UnaryExpr(ast) => self.lower_unary(ast),
                ast::Expr::VariableRef(ast) => self.lower_variable_ref(ast),
            }
        } else {
            Expr::Missing
        }
    }

    fn lower_literal(&self, ast: ast::Literal) -> Expr {
        match ast.kind() {
            ast::LiteralKind::Integer(int) => {
                if let Some(value) = int.value() {
                    Expr::Literal(Literal::Integer(value))
                } else {
                    Expr::Missing
                }
            }
            ast::LiteralKind::String(string) => {
                if let Some(value) = string.value() {
                    Expr::Literal(Literal::String(value))
                } else {
                    Expr::Missing
                }
            }
            ast::LiteralKind::Char(char) => {
                if let Some(c) = char.value() {
                    Expr::Literal(Literal::Char(c))
                } else {
                    Expr::Missing
                }
            }
            ast::LiteralKind::Bool(bool) => {
                if let Some(value) = bool.value() {
                    Expr::Literal(Literal::Bool(value))
                } else {
                    Expr::Missing
                }
            }
        }
    }

    fn lower_binary(&mut self, ast: ast::BinaryExpr) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Plus => BinaryOp::Add,
            SyntaxKind::Minus => BinaryOp::Sub,
            SyntaxKind::Star => BinaryOp::Mul,
            SyntaxKind::Slash => BinaryOp::Div,
            _ => unreachable!(),
        };

        let lhs = self.lower_expr(ast.lhs());
        let rhs = self.lower_expr(ast.rhs());

        Expr::Binary {
            op,
            lhs: self.exprs.alloc(lhs),
            rhs: self.exprs.alloc(rhs),
        }
    }

    fn lower_unary(&mut self, ast: ast::UnaryExpr) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Minus => UnaryOp::Neg,
            _ => unreachable!(),
        };

        let expr = self.lower_expr(ast.expr());

        Expr::Unary {
            op,
            expr: self.exprs.alloc(expr),
        }
    }

    fn lower_variable_ref(&mut self, ast: ast::VariableRef) -> Expr {
        Expr::VariableRef {
            var: ast.name().unwrap().text().into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> ast::SourceFile {
        ast::SourceFile::cast(parser::parse(input).syntax()).unwrap()
    }

    fn check_stmt(input: &str, expected_hir: Stmt, expected_database: Database) {
        let source_file = parse(input);
        let ast = source_file.stmts().next().unwrap();
        let mut database = Database::default();
        let hir = database.lower_stmt(ast).unwrap();

        assert_eq!(hir, expected_hir);
        assert_eq!(database, expected_database);
    }

    fn check_expr(input: &str, expected_hir: Expr, expected_database: Database) {
        let source_file = parse(input);
        let first_stmt = source_file.stmts().next().unwrap();
        let ast = match first_stmt {
            ast::Stmt::Expr(ast) => ast,
            _ => unreachable!(),
        };
        let mut database = Database::default();
        let hir = database.lower_expr(Some(ast));

        assert_eq!(hir, expected_hir);
        assert_eq!(database, expected_database);
    }

    #[test]
    fn lower_variable_def() {
        let mut exprs = Arena::new();
        let expr = exprs.alloc(Expr::VariableRef { var: "bar".into() });

        check_stmt(
            "let foo = bar",
            Stmt::VariableDef {
                name: "foo".into(),
                value: expr,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_variable_def_without_name() {
        let source_file = parse("let = 10");
        let ast = source_file.stmts().next().unwrap();
        assert!(Database::default().lower_stmt(ast).is_none());
    }

    #[test]
    fn lower_variable_def_without_value() {
        let mut exprs = Arena::new();
        let expr = exprs.alloc(Expr::Missing);

        check_stmt(
            "let a =",
            Stmt::VariableDef {
                name: "a".into(),
                value: expr,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_expr_stmt() {
        let mut exprs = Arena::new();
        let expr = exprs.alloc(Expr::Literal(Literal::Integer(123)));

        check_stmt("123", Stmt::Expr(expr), Database { exprs });
    }

    #[test]
    fn lower_binary_expr() {
        let mut exprs = Arena::new();
        let lhs = exprs.alloc(Expr::Literal(Literal::Integer(1)));
        let rhs = exprs.alloc(Expr::Literal(Literal::Integer(2)));

        check_expr(
            "1 + 2",
            Expr::Binary {
                lhs,
                rhs,
                op: BinaryOp::Add,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_binary_expr_without_rhs() {
        let mut exprs = Arena::new();

        check_expr(
            "10 -",
            Expr::Binary {
                lhs: exprs.alloc(Expr::Literal(Literal::Integer(10))),
                rhs: exprs.alloc(Expr::Missing),
                op: BinaryOp::Sub,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_integer_literal() {
        check_expr(
            "999",
            Expr::Literal(Literal::Integer(999)),
            Database::default(),
        );
    }

    #[test]
    fn lower_string_literal() {
        check_expr(
            "\"aaa\"",
            Expr::Literal(Literal::String("aaa".to_string())),
            Database::default(),
        );
    }

    #[test]
    fn lower_char_literal() {
        check_expr(
            "'a'",
            Expr::Literal(Literal::Char('a')),
            Database::default(),
        );
    }

    #[test]
    fn lower_bool_literal() {
        check_expr(
            "true",
            Expr::Literal(Literal::Bool(true)),
            Database::default(),
        );

        check_expr(
            "false",
            Expr::Literal(Literal::Bool(false)),
            Database::default(),
        );
    }

    #[test]
    fn lower_paren_expr() {
        check_expr(
            "((((((abc))))))",
            Expr::VariableRef { var: "abc".into() },
            Database::default(),
        );
    }

    #[test]
    fn lower_unary_expr() {
        let mut exprs = Arena::new();

        check_expr(
            "-10",
            Expr::Unary {
                expr: exprs.alloc(Expr::Literal(Literal::Integer(10))),
                op: UnaryOp::Neg,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_unary_expr_without_expr() {
        let mut exprs = Arena::new();

        check_expr(
            "-",
            Expr::Unary {
                expr: exprs.alloc(Expr::Missing),
                op: UnaryOp::Neg,
            },
            Database { exprs },
        );
    }

    #[test]
    fn lower_variable_ref() {
        check_expr(
            "foo",
            Expr::VariableRef { var: "foo".into() },
            Database::default(),
        );
    }
}
