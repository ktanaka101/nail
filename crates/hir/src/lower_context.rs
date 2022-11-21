mod scopes;

use la_arena::Arena;

use crate::lower_context::scopes::Scopes;
use crate::string_interner::Interner;
use crate::{BinaryOp, Expr, Literal, Name, Stmt, UnaryOp};

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
            ast::Stmt::FunctionDef(_) => return None,
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
                ast::Expr::Block(ast) => self.lower_block(ast),
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
            ast::BinaryOp::Add(_) => BinaryOp::Add,
            ast::BinaryOp::Sub(_) => BinaryOp::Sub,
            ast::BinaryOp::Mul(_) => BinaryOp::Mul,
            ast::BinaryOp::Div(_) => BinaryOp::Div,
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
            ast::UnaryOp::Neg(_) => UnaryOp::Neg,
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

        Expr::VariableRef { var: expr, name }
    }

    fn lower_block(&mut self, ast: ast::Block) -> Expr {
        self.scopes.enter();

        let mut stmts = vec![];
        for stmt in ast.stmts() {
            if let Some(stmt) = self.lower_stmt(stmt) {
                stmts.push(stmt);
            }
        }

        self.scopes.leave();

        Expr::Block { stmts }
    }
}

#[cfg(test)]
mod tests {
    use ast::AstNode;
    use expect_test::{expect, Expect};

    use crate::lower;

    use super::*;

    fn indent(nesting: usize) -> String {
        "    ".repeat(nesting)
    }

    fn debug(body: &[Stmt], db: &LowerContext) -> String {
        let mut msg = "".to_string();

        for stmt in body {
            msg.push_str(&debug_stmt(stmt, db, 0));
        }

        msg
    }

    fn debug_stmt(stmt: &Stmt, db: &LowerContext, nesting: usize) -> String {
        match stmt {
            Stmt::VariableDef { name, value } => {
                let name = db.interner.lookup(name.key());
                let expr_str = debug_expr(&db.exprs[*value], db, nesting);
                format!("{}let {} = {}\n", indent(nesting), name, expr_str)
            }
            Stmt::Expr(expr) => format!(
                "{}{}\n",
                indent(nesting),
                debug_expr(&db.exprs[*expr], db, nesting)
            ),
        }
    }

    fn debug_expr(expr: &Expr, db: &LowerContext, nesting: usize) -> String {
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
                let lhs_str = debug_expr(&db.exprs[*lhs], db, nesting);
                let rhs_str = debug_expr(&db.exprs[*rhs], db, nesting);
                format!("{} {} {}", lhs_str, op, rhs_str)
            }
            Expr::Unary { op, expr } => {
                let op = match op {
                    UnaryOp::Neg => "-",
                };
                let expr_str = debug_expr(&db.exprs[*expr], db, nesting);
                format!("{}{}", op, expr_str)
            }
            Expr::VariableRef { var, name } => match db.exprs[*var] {
                Expr::Binary { .. }
                | Expr::Missing
                | Expr::Literal(_)
                | Expr::Unary { .. }
                | Expr::VariableRef { .. } => debug_expr(&db.exprs[*var], db, nesting),
                Expr::Block { .. } => db.interner.lookup(name.key()).to_string(),
            },
            Expr::Block { stmts } => {
                let mut msg = "{\n".to_string();
                for stmt in stmts {
                    msg.push_str(&debug_stmt(stmt, db, nesting + 1));
                }
                msg.push_str(&format!("{}}}", indent(nesting)));

                msg
            }
            Expr::Missing => "<missing>".to_string(),
        }
    }

    fn parse(input: &str) -> ast::SourceFile {
        ast::SourceFile::cast(parser::parse(input).syntax()).unwrap()
    }

    fn check(input: &str, expected: Expect) {
        let source_file = parse(input);
        let result = lower(source_file);

        expected.assert_eq(&debug(&result.1, &result.0));
    }

    #[test]
    fn lower_variable_def() {
        check(
            r#"
                let foo = bar
            "#,
            expect![[r#"
                let foo = <missing>
            "#]],
        );
    }

    #[test]
    fn lower_variable_def_without_name() {
        check(
            r#"
                let = 10
            "#,
            expect![""],
        );
    }

    #[test]
    fn lower_variable_def_without_eq() {
        check(
            r#"
                let foo 10
            "#,
            expect![[r#"
                let foo = <missing>
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
                let a = <missing>
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
                123
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
                1 + 2
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
                10 - <missing>
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
                999
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
                "aaa"
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
                'a'
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
                true
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
                false
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
                <missing>
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
                -10
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
                -<missing>
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
                <missing>
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
                let foo = 10
                10
            "#]],
        );
    }

    #[test]
    fn lower_block() {
        check(
            r#"
                {
                    let foo = 10;
                }
            "#,
            expect![[r#"
                {
                    let foo = 10
                }
            "#]],
        );
    }

    #[test]
    fn lower_nested_block() {
        check(
            r#"
                {
                    let a = 10
                    {
                        let b = 20
                        let c = 30
                    }
                    let d = 40
                }
            "#,
            expect![[r#"
                {
                    let a = 10
                    {
                        let b = 20
                        let c = 30
                    }
                    let d = 40
                }
            "#]],
        );
    }

    #[test]
    fn lower_with_scope() {
        check(
            r#"
                {
                    let a = 10
                }
                a
            "#,
            expect![[r#"
                {
                    let a = 10
                }
                <missing>
            "#]],
        );
    }

    #[test]
    fn lower_with_nested_scope() {
        check(
            r#"
                let a = 10
                {
                    let b = 20
                    {
                        let c = 30
                        a + b + c
                    }
                }
                a
            "#,
            expect![[r#"
                let a = 10
                {
                    let b = 20
                    {
                        let c = 30
                        10 + 20 + 30
                    }
                }
                10
            "#]],
        );
    }

    #[test]
    fn shadwing() {
        check(
            r#"
                let a = 10
                a
                let a = 20
                a
            "#,
            expect![[r#"
                let a = 10
                10
                let a = 20
                20
            "#]],
        );
    }

    #[test]
    fn shadwing_on_block() {
        check(
            r#"
                a
                let a = 10
                a
                {
                    a
                    let a = 20
                    {
                        a
                        let a = 30
                        a
                    }
                    a
                }
                a
            "#,
            expect![[r#"
                <missing>
                let a = 10
                10
                {
                    10
                    let a = 20
                    {
                        20
                        let a = 30
                        30
                    }
                    20
                }
                10
            "#]],
        );
    }

    #[test]
    fn can_bind_block_because_it_is_expr() {
        check(
            r#"
                let a = {
                    a
                    let a = 10;
                    a
                };
                a

                20 + {
                    let b = 30
                    a + b
                }
            "#,
            expect![[r#"
                let a = {
                    <missing>
                    let a = 10
                    10
                }
                a
                20 + {
                    let b = 30
                    a + 30
                }
            "#]],
        );
    }
}
