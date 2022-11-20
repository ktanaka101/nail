use std::collections::HashMap;

use la_arena::Arena;

use crate::interner::{Interner, Key};
use crate::{BinaryOp, Expr, ExprIdx, Literal, Stmt, UnaryOp};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Key);
impl Name {
    pub fn key(&self) -> Key {
        self.0
    }

    fn from_key(key: Key) -> Self {
        Self(key)
    }
}

#[derive(Debug)]
struct Scopes {
    inner: Vec<HashMap<Name, ExprIdx>>,
}
impl Scopes {
    pub(crate) fn new() -> Self {
        let mut scopes = Self { inner: Vec::new() };
        scopes.enter();

        scopes
    }

    pub(crate) fn get(&self, name: Name) -> Option<ExprIdx> {
        assert!(!self.inner.is_empty());

        for scope in self.inner.iter().rev() {
            if let Some(idx) = scope.get(&name) {
                return Some(idx.to_owned());
            }
        }

        None
    }

    pub(crate) fn push(&mut self, name: Name, value: ExprIdx) {
        assert!(!self.inner.is_empty());

        self.inner.last_mut().unwrap().insert(name, value);
    }

    pub(crate) fn enter(&mut self) {
        self.inner.push(HashMap::default());
    }
}

#[derive(Debug)]
pub struct LowerContext {
    pub exprs: Arena<Expr>,
    scopes: Scopes,
    interner: Interner,
}

impl LowerContext {
    pub(super) fn new() -> Self {
        Self {
            exprs: Arena::new(),
            scopes: Scopes::new(),
            interner: Interner::new(),
        }
    }

    pub(super) fn lower_stmt(&mut self, ast: ast::Stmt) -> Option<Stmt> {
        let result = match ast {
            ast::Stmt::VariableDef(def) => {
                let expr = self.lower_expr(def.value());
                let idx = self.exprs.alloc(expr);
                let name = Name::from_key(self.interner.intern(def.name()?.name()));
                self.scopes.push(name, idx);
                Stmt::VariableDef { name, value: idx }
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
        let op = match ast.op().unwrap() {
            ast::BinaryOp::Add => BinaryOp::Add,
            ast::BinaryOp::Sub => BinaryOp::Sub,
            ast::BinaryOp::Mul => BinaryOp::Mul,
            ast::BinaryOp::Div => BinaryOp::Div,
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
        let op = match ast.op().unwrap() {
            ast::UnaryOp::Neg => UnaryOp::Neg,
        };

        let expr = self.lower_expr(ast.expr());

        Expr::Unary {
            op,
            expr: self.exprs.alloc(expr),
        }
    }

    fn lower_variable_ref(&mut self, ast: ast::VariableRef) -> Expr {
        let name = Name::from_key(self.interner.intern(ast.name().unwrap().name()));
        let expr = if let Some(expr) = self.scopes.get(name) {
            expr
        } else {
            self.exprs.alloc(Expr::Missing)
        };

        Expr::VariableRef { var: expr }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::lower;

    use super::*;

    fn debug_ref(body: &[Stmt], db: &LowerContext) -> String {
        let mut msg = "".to_string();

        for stmt in body {
            match stmt {
                Stmt::VariableDef { name, value } => {
                    let name = db.interner.lookup(name.key());
                    msg.push_str(&format!("let {} = %{}\n", name, value.into_raw()));
                }
                Stmt::Expr(expr) => {
                    msg.push_str(&format!("%{}\n", expr.into_raw()));
                }
            }
        }

        msg.push('\n');
        msg.push_str("---\n");

        for expr in db.exprs.iter() {
            msg.push_str(&format!("{}: {}\n", expr.0.into_raw(), debug_expr(expr.1)));
        }

        msg
    }

    fn debug_expr(expr: &Expr) -> String {
        match expr {
            Expr::Literal(literal) => match literal {
                Literal::Bool(b) => b.to_string(),
                Literal::Char(c) => format!("'{}'", c),
                Literal::String(s) => format!("\"{}\"", s),
                Literal::Integer(i) => i.to_string(),
            },
            Expr::Binary { op, lhs, rhs } => {
                let op = match op {
                    BinaryOp::Add => "+",
                    BinaryOp::Sub => "-",
                    BinaryOp::Mul => "*",
                    BinaryOp::Div => "/",
                };
                format!("%{} {} %{}", lhs.into_raw(), op, rhs.into_raw())
            }
            Expr::Unary { op, expr } => {
                let op = match op {
                    UnaryOp::Neg => "-",
                };
                format!("{}%{}", op, expr.into_raw())
            }
            Expr::VariableRef { var } => format!("%{}", var.into_raw()),
            Expr::Missing => "missing".to_string(),
        }
    }

    fn parse(input: &str) -> ast::SourceFile {
        ast::SourceFile::cast(parser::parse(input).syntax()).unwrap()
    }

    fn check(input: &str, expected: Expect) {
        let source_file = parse(input);
        let result = lower(source_file);

        expected.assert_eq(&debug_ref(&result.1, &result.0));
    }

    #[test]
    fn lower_variable_def() {
        check(
            r#"
                let foo = bar
            "#,
            expect![[r#"
                let foo = %1

                ---
                0: missing
                1: %0
            "#]],
        );
    }

    #[test]
    fn lower_variable_def_without_name() {
        check(
            r#"
                let = 10
            "#,
            expect![[r#"

                ---
                0: 10
            "#]],
        );
    }

    #[test]
    fn lower_variable_def_without_eq() {
        check(
            r#"
                let foo 10
            "#,
            expect![[r#"
                let foo = %0

                ---
                0: missing
            "#]],
        );
    }

    #[test]
    fn lower_variable_def_without_value() {
        check(
            r#"
                let a =
            "#,
            expect![[r#"
                let a = %0

                ---
                0: missing
            "#]],
        );
    }

    #[test]
    fn lower_expr_stmt() {
        check(
            r#"
                123
            "#,
            expect![[r#"
                %0

                ---
                0: 123
            "#]],
        );
    }

    #[test]
    fn lower_binary_expr() {
        check(
            r#"
                1 + 2
            "#,
            expect![[r#"
                %2

                ---
                0: 1
                1: 2
                2: %0 + %1
            "#]],
        );
    }

    #[test]
    fn lower_binary_expr_without_rhs() {
        check(
            r#"
                10 -
            "#,
            expect![[r#"
                %2

                ---
                0: 10
                1: missing
                2: %0 - %1
            "#]],
        );
    }

    #[test]
    fn lower_integer_literal() {
        check(
            r#"
                999
            "#,
            expect![[r#"
                %0

                ---
                0: 999
            "#]],
        );
    }

    #[test]
    fn lower_string_literal() {
        check(
            r#"
                "aaa"
            "#,
            expect![[r#"
                %0

                ---
                0: "aaa"
            "#]],
        );
    }

    #[test]
    fn lower_char_literal() {
        check(
            r#"
                'a'
            "#,
            expect![[r#"
                %0

                ---
                0: 'a'
            "#]],
        );
    }

    #[test]
    fn lower_bool_literal_true() {
        check(
            r#"
                true
            "#,
            expect![[r#"
                %0

                ---
                0: true
            "#]],
        );
    }

    #[test]
    fn lower_bool_literal_false() {
        check(
            r#"
                false
            "#,
            expect![[r#"
                %0

                ---
                0: false
            "#]],
        );
    }

    #[test]
    fn lower_paren_expr() {
        check(
            r#"
                ((((((abc))))))
            "#,
            expect![[r#"
                %1

                ---
                0: missing
                1: %0
            "#]],
        );
    }

    #[test]
    fn lower_unary_expr() {
        check(
            r#"
                -10
            "#,
            expect![[r#"
                %1

                ---
                0: 10
                1: -%0
            "#]],
        );
    }

    #[test]
    fn lower_unary_expr_without_expr() {
        check(
            r#"
                -
            "#,
            expect![[r#"
                %1

                ---
                0: missing
                1: -%0
            "#]],
        );
    }

    #[test]
    fn lower_variable_ref() {
        check(
            r#"
                foo
            "#,
            expect![[r#"
                %1

                ---
                0: missing
                1: %0
            "#]],
        );
    }

    #[test]
    fn lookup_variable_ref() {
        check(
            r#"
                let foo = 10
                foo
            "#,
            expect![[r#"
                let foo = %0
                %1

                ---
                0: 10
                1: %0
            "#]],
        );
    }
}
