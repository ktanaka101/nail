use syntax::SyntaxKind;

use crate::{BinaryOp, Expr, Stmt, UnaryOp};
use la_arena::Arena;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Database {
    pub exprs: Arena<Expr>,
}

impl Database {
    pub(super) fn lower_stmt(&mut self, ast: ast::Stmt) -> Option<Stmt> {
        let result = match ast {
            ast::Stmt::VariableDef(ast) => Stmt::VariableDef {
                name: ast.name()?.text().into(),
                value: self.lower_expr(ast.value()),
            },
            ast::Stmt::Expr(ast) => Stmt::Expr(self.lower_expr(Some(ast))),
        };

        Some(result)
    }

    fn lower_expr(&mut self, ast: Option<ast::Expr>) -> Expr {
        if let Some(ast) = ast {
            match ast {
                ast::Expr::BinaryExpr(ast) => self.lower_binary(ast),
                ast::Expr::Literal(ast) => Expr::Literal { n: ast.parse() },
                ast::Expr::ParenExpr(ast) => self.lower_expr(ast.expr()),
                ast::Expr::UnaryExpr(ast) => self.lower_unary(ast),
                ast::Expr::VariableRef(ast) => self.lower_variable_ref(ast),
            }
        } else {
            Expr::Missing
        }
    }

    fn lower_binary(&mut self, ast: ast::BinaryExpr) -> Expr {
        let op = match ast.op().unwrap().kind() {
            SyntaxKind::Plus => BinaryOp::Add,
            SyntaxKind::Minus => BinaryOp::Sub,
            SyntaxKind::Asterisk => BinaryOp::Mul,
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

    fn parse(input: &str) -> ast::Root {
        ast::Root::cast(parser::parse(input).syntax()).unwrap()
    }

    fn check_stmt(input: &str, expected_hir: Stmt) {
        let root = parse(input);
        let ast = root.stmts().next().unwrap();
        let hir = Database::default().lower_stmt(ast).unwrap();

        assert_eq!(hir, expected_hir);
    }

    fn check_expr(input: &str, expected_hir: Expr, expected_database: Database) {
        let root = parse(input);
        let first_stmt = root.stmts().next().unwrap();
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
        check_stmt(
            "let foo = bar",
            Stmt::VariableDef {
                name: "foo".into(),
                value: Expr::VariableRef { var: "bar".into() },
            },
        );
    }
}
