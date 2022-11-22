mod scopes;

use std::collections::HashMap;

use la_arena::{Arena, Idx};

use crate::body::scopes::Scopes;
use crate::string_interner::Interner;
use crate::{BinaryOp, Block, Expr, Literal, Name, Stmt, Symbol, UnaryOp};

#[derive(Debug)]
struct FunctionScopes {
    inner: HashMap<Name, BodyLowerContextIdx>,
}
impl FunctionScopes {
    fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    fn insert(&mut self, key: Name, ctx: BodyLowerContextIdx) {
        self.inner.insert(key, ctx);
    }

    fn get(&self, key: &Name) -> Option<&BodyLowerContextIdx> {
        self.inner.get(key)
    }
}

#[derive(Debug)]
pub struct BodyLowerContext {
    pub exprs: Arena<Expr>,
    scopes: Scopes,
    interner: Interner,
    function_scopes: FunctionScopes,
    context_arena: Arena<BodyLowerContext>,
    params: Vec<Name>,
}

type BodyLowerContextIdx = Idx<BodyLowerContext>;

impl BodyLowerContext {
    pub(super) fn new() -> Self {
        Self {
            exprs: Arena::new(),
            scopes: Scopes::new(),
            interner: Interner::new(),
            function_scopes: FunctionScopes::new(),
            context_arena: Arena::new(),
            params: Vec::new(),
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
            ast::Stmt::FunctionDef(def) => {
                let mut body_lower_ctx = BodyLowerContext::new();
                let name = Name::from_key(self.interner.intern(def.name()?.name()));
                let stmt = body_lower_ctx.lower_function(name, def)?;

                let ctx_idx = self.context_arena.alloc(body_lower_ctx);
                self.function_scopes.insert(name, ctx_idx);

                stmt
            }
        };

        Some(result)
    }

    fn lower_function(&mut self, name: Name, ast: ast::FunctionDef) -> Option<Stmt> {
        let params = ast
            .params()?
            .params()
            .filter_map(|it| it.name())
            .map(|it| Name::from_key(self.interner.intern(it.name())))
            .collect::<Vec<_>>();

        let body = ast.body()?;
        let block_idx = self.lower_block(body);
        let body_idx = self.exprs.alloc(block_idx);

        Some(Stmt::FunctionDef {
            name,
            params,
            body: body_idx,
        })
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
        let symbol = if let Some(expr) = self.scopes.get(name) {
            Symbol::Local(expr)
        } else if self.params.iter().any(|param| *param == name) {
            Symbol::Param
        } else {
            Symbol::Missing
        };

        Expr::VariableRef { var: symbol, name }
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

        let tail = if let Some(Stmt::Expr(expr)) = stmts.last() {
            let expr = *expr;
            stmts.pop();
            Some(expr)
        } else {
            None
        };

        Expr::Block(Block { stmts, tail })
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

    fn debug(stmts: &[Stmt], ctx: &BodyLowerContext) -> String {
        let mut msg = "".to_string();

        for stmt in stmts {
            msg.push_str(&debug_stmt(stmt, ctx, 0));
        }

        msg
    }

    fn debug_stmt(stmt: &Stmt, ctx: &BodyLowerContext, nesting: usize) -> String {
        match stmt {
            Stmt::VariableDef { name, value } => {
                let name = ctx.interner.lookup(name.key());
                let expr_str = debug_expr(&ctx.exprs[*value], ctx, nesting);
                format!("{}let {} = {}\n", indent(nesting), name, expr_str)
            }
            Stmt::Expr(expr) => format!(
                "{}{}\n",
                indent(nesting),
                debug_expr(&ctx.exprs[*expr], ctx, nesting)
            ),
            Stmt::FunctionDef { name, params, body } => {
                let fn_ctx_idx = ctx.function_scopes.get(name).unwrap();
                let fn_ctx = &ctx.context_arena[*fn_ctx_idx];

                let name = ctx.interner.lookup(name.key());
                let params = params
                    .iter()
                    .map(|name| fn_ctx.interner.lookup(name.key()))
                    .collect::<Vec<_>>()
                    .join(", ");

                let body = &fn_ctx.exprs[*body];
                let body = debug_expr(body, fn_ctx, nesting);
                format!("{}fn {}({}) {}\n", indent(nesting), name, params, body,)
            }
        }
    }

    fn debug_expr(expr: &Expr, ctx: &BodyLowerContext, nesting: usize) -> String {
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
                let lhs_str = debug_expr(&ctx.exprs[*lhs], ctx, nesting);
                let rhs_str = debug_expr(&ctx.exprs[*rhs], ctx, nesting);
                format!("{} {} {}", lhs_str, op, rhs_str)
            }
            Expr::Unary { op, expr } => {
                let op = match op {
                    UnaryOp::Neg => "-",
                };
                let expr_str = debug_expr(&ctx.exprs[*expr], ctx, nesting);
                format!("{}{}", op, expr_str)
            }
            Expr::VariableRef { var, name } => match var {
                Symbol::Local(expr) => match ctx.exprs[*expr] {
                    Expr::Binary { .. }
                    | Expr::Missing
                    | Expr::Literal(_)
                    | Expr::Unary { .. }
                    | Expr::VariableRef { .. } => debug_expr(&ctx.exprs[*expr], ctx, nesting),
                    Expr::Block { .. } => ctx.interner.lookup(name.key()).to_string(),
                },
                Symbol::Param => {
                    let name = ctx.interner.lookup(name.key());
                    format!("param:{}", name)
                }
                Symbol::Missing => "<missing>".to_string(),
            },
            Expr::Block(block) => {
                let mut msg = "{\n".to_string();
                for stmt in &block.stmts {
                    msg.push_str(&debug_stmt(stmt, ctx, nesting + 1));
                }
                if let Some(tail) = block.tail {
                    msg.push_str(&format!(
                        "{}expr:{}\n",
                        indent(nesting + 1),
                        debug_expr(&ctx.exprs[tail], ctx, nesting + 1)
                    ));
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
                    expr:{
                        let c = 30
                        expr:10 + 20 + 30
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
                        expr:30
                    }
                    expr:20
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
                    expr:10
                }
                a
                20 + {
                    let b = 30
                    expr:a + 30
                }
            "#]],
        );
    }

    #[test]
    fn define_function() {
        check(
            r#"
                fn foo() {
                    let a = 10;
                    a
                }
            "#,
            expect![[r#"
                fn foo() {
                    let a = 10
                    expr:10
                }
            "#]],
        );
    }

    #[test]
    fn function_scope() {
        check(
            r#"
                let a = 10;
                fn foo() {
                    a
                    let a = 20;
                    a
                }
            "#,
            expect![[r#"
                let a = 10
                fn foo() {
                    <missing>
                    let a = 20
                    expr:20
                }
            "#]],
        );
    }

    #[test]
    fn define_function_in_block() {
        check(
            r#"
                {
                    fn foo() {
                        let a = 10;
                        {
                            let b = 20;
                            fn bar() {
                                let c = 20;
                                fn baz() {
                                    a + b + c
                                }
                                a + b + c
                            }
                        }
                    }
                }
            "#,
            expect![[r#"
                {
                    fn foo() {
                        let a = 10
                        expr:{
                            let b = 20
                            fn bar() {
                                let c = 20
                                fn baz() {
                                    expr:<missing> + <missing> + <missing>
                                }
                                expr:<missing> + <missing> + 20
                            }
                        }
                    }
                }
            "#]],
        );
    }

    #[test]
    fn shadowing_in_function() {
        check(
            r#"
                a
                let a = 10;
                a
                fn foo() {
                    a
                    let a = 20;
                    a
                    {
                        a
                        let a = 30;
                        a
                    }
                    a
                    fn bar() {
                        a
                        let a = 40;
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
                fn foo() {
                    <missing>
                    let a = 20
                    20
                    {
                        20
                        let a = 30
                        expr:30
                    }
                    20
                    fn bar() {
                        <missing>
                        let a = 40
                        expr:40
                    }
                    expr:20
                }
                10
            "#]],
        );
    }
}
