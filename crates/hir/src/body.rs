mod scopes;

use std::collections::HashMap;

use la_arena::{Arena, Idx};

use crate::body::scopes::Scopes;
use crate::item_tree::{Database, ItemTree};
use crate::string_interner::Interner;
use crate::{AstId, BinaryOp, Block, Expr, Literal, Name, Stmt, Symbol, UnaryOp};

use self::scopes::CurrentBlock;

#[derive(Debug, Default)]
pub struct RootBodyLowerContext {
    pub function_bodies: Arena<Expr>,
    pub function_body_context_mapping: HashMap<AstId<ast::Block>, Idx<BodyLowerContext>>,
    pub function_body_expr_mapping: HashMap<AstId<ast::Block>, Idx<Expr>>,
    pub context_arena: Arena<BodyLowerContext>,
}
impl RootBodyLowerContext {
    pub fn new() -> Self {
        Self {
            function_bodies: Arena::new(),
            function_body_context_mapping: HashMap::new(),
            function_body_expr_mapping: HashMap::new(),
            context_arena: Arena::new(),
        }
    }
}

#[derive(Debug)]
pub struct BodyLowerContext {
    pub exprs: Arena<Expr>,
    scopes: Scopes,
    params: Vec<Name>,
}

impl BodyLowerContext {
    pub(super) fn new(params: Vec<Name>) -> Self {
        Self {
            exprs: Arena::new(),
            scopes: Scopes::new(),
            params,
        }
    }

    pub(super) fn lower_stmt(
        &mut self,
        ast: ast::Stmt,
        ctx: &mut RootBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Option<Stmt> {
        let result = match ast {
            ast::Stmt::VariableDef(def) => {
                let expr = self.lower_expr(def.value(), ctx, db, item_tree, interner);
                let idx = self.exprs.alloc(expr);
                let name = Name::from_key(interner.intern(def.name()?.name()));
                self.scopes.push(name, idx);
                Stmt::VariableDef { name, value: idx }
            }
            ast::Stmt::Expr(ast) => {
                let expr = self.lower_expr(Some(ast), ctx, db, item_tree, interner);
                Stmt::Expr(self.exprs.alloc(expr))
            }
            ast::Stmt::FunctionDef(def) => {
                let params = def
                    .params()?
                    .params()
                    .filter_map(|param| param.name())
                    .map(|ident| Name::from_key(interner.intern(ident.name())))
                    .collect();
                let mut body_lower_ctx = BodyLowerContext::new(params);
                let name = Name::from_key(interner.intern(def.name()?.name()));
                let body = def.body()?;
                let stmt =
                    body_lower_ctx.lower_function(name, def, ctx, db, item_tree, interner)?;

                let ctx_idx = ctx.context_arena.alloc(body_lower_ctx);
                ctx.function_body_context_mapping
                    .insert(db.lookup_ast_id(&body).unwrap(), ctx_idx);

                stmt
            }
        };

        Some(result)
    }

    fn lower_function(
        &mut self,
        name: Name,
        ast: ast::FunctionDef,
        ctx: &mut RootBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Option<Stmt> {
        let params = ast
            .params()?
            .params()
            .filter_map(|it| it.name())
            .map(|it| Name::from_key(interner.intern(it.name())))
            .collect::<Vec<_>>();

        let body = ast.body()?;
        let ast_id = db.lookup_ast_id(&body).unwrap();
        let block = self.lower_block(body, ctx, db, item_tree, interner);
        let body_idx = ctx.function_bodies.alloc(block);
        ctx.function_body_expr_mapping.insert(ast_id, body_idx);

        Some(Stmt::FunctionDef {
            name,
            params,
            body: body_idx,
        })
    }

    fn lower_expr(
        &mut self,
        ast: Option<ast::Expr>,
        ctx: &mut RootBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Expr {
        if let Some(ast) = ast {
            match ast {
                ast::Expr::BinaryExpr(ast) => self.lower_binary(ast, ctx, db, item_tree, interner),
                ast::Expr::Literal(ast) => self.lower_literal(ast),
                ast::Expr::ParenExpr(ast) => {
                    self.lower_expr(ast.expr(), ctx, db, item_tree, interner)
                }
                ast::Expr::UnaryExpr(ast) => self.lower_unary(ast, ctx, db, item_tree, interner),
                ast::Expr::VariableRef(ast) => {
                    self.lower_variable_ref(ast, ctx, db, item_tree, interner)
                }
                ast::Expr::Block(ast) => self.lower_block(ast, ctx, db, item_tree, interner),
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

    fn lower_binary(
        &mut self,
        ast: ast::BinaryExpr,
        ctx: &mut RootBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Expr {
        let op = match ast.op().unwrap() {
            ast::BinaryOp::Add(_) => BinaryOp::Add,
            ast::BinaryOp::Sub(_) => BinaryOp::Sub,
            ast::BinaryOp::Mul(_) => BinaryOp::Mul,
            ast::BinaryOp::Div(_) => BinaryOp::Div,
        };

        let lhs = self.lower_expr(ast.lhs(), ctx, db, item_tree, interner);
        let rhs = self.lower_expr(ast.rhs(), ctx, db, item_tree, interner);

        Expr::Binary {
            op,
            lhs: self.exprs.alloc(lhs),
            rhs: self.exprs.alloc(rhs),
        }
    }

    fn lower_unary(
        &mut self,
        ast: ast::UnaryExpr,
        ctx: &mut RootBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Expr {
        let op = match ast.op().unwrap() {
            ast::UnaryOp::Neg(_) => UnaryOp::Neg,
        };

        let expr = self.lower_expr(ast.expr(), ctx, db, item_tree, interner);

        Expr::Unary {
            op,
            expr: self.exprs.alloc(expr),
        }
    }

    fn lower_variable_ref(
        &mut self,
        ast: ast::VariableRef,
        _ctx: &mut RootBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Expr {
        let name = Name::from_key(interner.intern(ast.name().unwrap().name()));
        let symbol = if let Some(expr) = self.scopes.get_from_current_scope(name) {
            Symbol::Local(expr)
        } else if self.params.iter().any(|param| *param == name) {
            Symbol::Param
        } else {
            let item_scope = match self.scopes.current_block() {
                CurrentBlock::Root => item_tree.root_scope(db),
                CurrentBlock::Block(block_idx) => item_tree.block_scope(db, block_idx).unwrap(),
            };
            if let Some(function) =
                item_scope.lookup(ast.name().unwrap().name(), db, item_tree, interner)
            {
                Symbol::Function(function)
            } else if let Some(expr) = self.scopes.get(name) {
                Symbol::Local(expr)
            } else {
                Symbol::Missing
            }
        };

        Expr::VariableRef { var: symbol, name }
    }

    fn lower_block(
        &mut self,
        ast: ast::Block,
        ctx: &mut RootBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Expr {
        let block_id = db.lookup_ast_id(&ast).unwrap();
        self.scopes.enter(CurrentBlock::Block(block_id.clone()));

        let mut stmts = vec![];
        for stmt in ast.stmts() {
            if let Some(stmt) = self.lower_stmt(stmt, ctx, db, item_tree, interner) {
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

        Expr::Block(Block {
            stmts,
            tail,
            ast: block_id,
        })
    }
}

#[cfg(test)]
mod tests {
    use ast::AstNode;
    use expect_test::{expect, Expect};

    use crate::{item_tree::Function, lower};

    use super::*;

    fn indent(nesting: usize) -> String {
        "    ".repeat(nesting)
    }

    fn debug(
        stmts: &[Stmt],
        root_ctx: &RootBodyLowerContext,
        ctx: &BodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &Interner,
    ) -> String {
        let mut msg = "".to_string();

        for stmt in stmts {
            msg.push_str(&debug_stmt(stmt, root_ctx, ctx, db, item_tree, interner, 0));
        }

        msg
    }

    fn debug_function(
        function_idx: Idx<Function>,
        root_ctx: &RootBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &Interner,
        nesting: usize,
    ) -> String {
        let function = &db.functions[function_idx];
        let block_id = item_tree.function_to_block(&function_idx).unwrap();
        let function_ctx = root_ctx
            .function_body_context_mapping
            .get(&block_id)
            .unwrap();
        let function_ctx = &root_ctx.context_arena[*function_ctx];
        let body_expr = root_ctx.function_body_expr_mapping.get(&block_id).unwrap();
        let body_expr = &root_ctx.function_bodies[*body_expr];

        let name = interner.lookup(function.name.key());
        let params = function
            .params
            .iter()
            .map(|param| interner.lookup(param.name.key()))
            .collect::<Vec<_>>()
            .join(", ");

        let body = debug_expr(
            body_expr,
            root_ctx,
            function_ctx,
            db,
            item_tree,
            interner,
            nesting,
        );
        format!("{}fn {}({}) {}\n", indent(nesting), name, params, body)
    }

    fn debug_stmt(
        stmt: &Stmt,
        root_ctx: &RootBodyLowerContext,
        ctx: &BodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &Interner,
        nesting: usize,
    ) -> String {
        match stmt {
            Stmt::VariableDef { name, value } => {
                let name = interner.lookup(name.key());
                let expr_str = debug_expr(
                    &ctx.exprs[*value],
                    root_ctx,
                    ctx,
                    db,
                    item_tree,
                    interner,
                    nesting,
                );
                format!("{}let {} = {}\n", indent(nesting), name, expr_str)
            }
            Stmt::Expr(expr) => format!(
                "{}{}\n",
                indent(nesting),
                debug_expr(
                    &ctx.exprs[*expr],
                    root_ctx,
                    ctx,
                    db,
                    item_tree,
                    interner,
                    nesting
                )
            ),
            Stmt::FunctionDef { body, .. } => {
                let body = &root_ctx.function_bodies[*body];
                if let Expr::Block(block) = body {
                    let function_idx = item_tree.block_to_function(&block.ast).unwrap();
                    debug_function(function_idx, root_ctx, db, item_tree, interner, nesting)
                } else {
                    panic!("supported only block");
                }
            }
        }
    }

    fn debug_expr(
        expr: &Expr,
        root_ctx: &RootBodyLowerContext,
        ctx: &BodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &Interner,
        nesting: usize,
    ) -> String {
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
                let lhs_str = debug_expr(
                    &ctx.exprs[*lhs],
                    root_ctx,
                    ctx,
                    db,
                    item_tree,
                    interner,
                    nesting,
                );
                let rhs_str = debug_expr(
                    &ctx.exprs[*rhs],
                    root_ctx,
                    ctx,
                    db,
                    item_tree,
                    interner,
                    nesting,
                );
                format!("{} {} {}", lhs_str, op, rhs_str)
            }
            Expr::Unary { op, expr } => {
                let op = match op {
                    UnaryOp::Neg => "-",
                };
                let expr_str = debug_expr(
                    &ctx.exprs[*expr],
                    root_ctx,
                    ctx,
                    db,
                    item_tree,
                    interner,
                    nesting,
                );
                format!("{}{}", op, expr_str)
            }
            Expr::VariableRef { var, name } => match var {
                Symbol::Local(expr) => match ctx.exprs[*expr] {
                    Expr::Binary { .. }
                    | Expr::Missing
                    | Expr::Literal(_)
                    | Expr::Unary { .. }
                    | Expr::VariableRef { .. } => debug_expr(
                        &ctx.exprs[*expr],
                        root_ctx,
                        ctx,
                        db,
                        item_tree,
                        interner,
                        nesting,
                    ),
                    Expr::Block { .. } => interner.lookup(name.key()).to_string(),
                },
                Symbol::Param => {
                    let name = interner.lookup(name.key());
                    format!("param:{}", name)
                }
                Symbol::Function(_) => {
                    let name = interner.lookup(name.key());
                    format!("fn:{}", name)
                }
                Symbol::Missing => "<missing>".to_string(),
            },
            Expr::Block(block) => {
                let mut msg = "{\n".to_string();
                for stmt in &block.stmts {
                    msg.push_str(&debug_stmt(
                        stmt,
                        root_ctx,
                        ctx,
                        db,
                        item_tree,
                        interner,
                        nesting + 1,
                    ));
                }
                if let Some(tail) = block.tail {
                    msg.push_str(&format!(
                        "{}expr:{}\n",
                        indent(nesting + 1),
                        debug_expr(
                            &ctx.exprs[tail],
                            root_ctx,
                            ctx,
                            db,
                            item_tree,
                            interner,
                            nesting + 1
                        )
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

        expected.assert_eq(&debug(
            &result.2, &result.0, &result.1, &result.3, &result.4, &result.5,
        ));
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
    fn can_use_block_in_binary_expr() {
        check(
            r#"
                {
                    let a = 10;
                    a
                } + {
                    let b = 30
                    a + b
                }
            "#,
            expect![[r#"
                {
                    let a = 10
                    expr:10
                } + {
                    let b = 30
                    expr:<missing> + 30
                }
            "#]],
        );
    }

    #[test]
    fn can_use_block_in_paren_expr() {
        check(
            r#"
            (
                (
                    {
                        let a = 10;
                        (
                            {
                                a
                            }
                        )
                    }
                )
            )
            "#,
            expect![[r#"
                {
                    let a = 10
                    expr:{
                        expr:10
                    }
                }
            "#]],
        );
    }

    #[test]
    fn can_use_block_in_unary_expr() {
        check(
            r#"
                -{
                    let a = 10;
                    -{
                        -a
                    }
                }
            "#,
            expect![[r#"
                -{
                    let a = 10
                    expr:-{
                        expr:-10
                    }
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
    fn define_function_with_params() {
        check(
            r#"
                fn foo(a, b) {
                    1
                }
            "#,
            expect![[r#"
                fn foo(a, b) {
                    expr:1
                }
            "#]],
        );
    }

    #[test]
    fn resolve_function_params() {
        check(
            r#"
                fn foo(a, b) {
                    a + b
                }
            "#,
            expect![[r#"
                fn foo(a, b) {
                    expr:param:a + param:b
                }
            "#]],
        );
    }

    #[test]
    fn resolve_function_params_by_nested_scope() {
        check(
            r#"
                fn foo(a, b) {
                    let a = 0;
                    a
                    b
                    fn bar(a, b) {
                        {
                            a + b
                        }
                    }
                    a
                    b
                }
                a
                b
            "#,
            expect![[r#"
                fn foo(a, b) {
                    let a = 0
                    0
                    param:b
                    fn bar(a, b) {
                        expr:{
                            expr:param:a + param:b
                        }
                    }
                    0
                    expr:param:b
                }
                <missing>
                <missing>
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

    #[test]
    fn ref_function_from_outer_scope() {
        check(
            r#"
                fn foo() {
                    foo
                    bar
                    baz
                    fn bar() {
                        foo
                        bar
                        baz
                    }
                }
                fn baz() {
                    foo
                    bar
                    baz
                }
                foo
                bar
                baz
            "#,
            expect![[r#"
                fn foo() {
                    fn:foo
                    fn:bar
                    fn:baz
                    fn bar() {
                        fn:foo
                        fn:bar
                        expr:fn:baz
                    }
                }
                fn baz() {
                    fn:foo
                    <missing>
                    expr:fn:baz
                }
                fn:foo
                <missing>
                fn:baz
            "#]],
        );
    }
}
