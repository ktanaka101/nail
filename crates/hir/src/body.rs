mod scopes;

use std::collections::HashMap;

use la_arena::{Arena, Idx};

use self::scopes::ScopeType;
use crate::{
    body::scopes::Scopes, db::Database, item_tree::ItemTree, string_interner::Interner, AstId,
    BinaryOp, Block, Expr, ExprIdx, Literal, Name, ParamIdx, Stmt, Symbol, UnaryOp,
};

#[derive(Debug, Default)]
pub struct SharedBodyLowerContext {
    function_bodies: Arena<Expr>,
    pub exprs: Arena<Expr>,
    function_body_by_block: HashMap<AstId<ast::Block>, ExprIdx>,
}
impl SharedBodyLowerContext {
    pub fn new() -> Self {
        Self {
            function_bodies: Arena::new(),
            exprs: Arena::new(),
            function_body_by_block: HashMap::new(),
        }
    }

    pub fn function_body_idx_by_block(
        &self,
        block_ast_id: &AstId<ast::Block>,
    ) -> Option<Idx<Expr>> {
        self.function_body_by_block.get(block_ast_id).copied()
    }

    pub fn function_body_by_block(&self, block_ast_id: &AstId<ast::Block>) -> Option<&Expr> {
        self.function_body_idx_by_block(block_ast_id)
            .map(|idx| &self.function_bodies[idx])
    }
}

#[derive(Debug)]
pub struct BodyLower {
    scopes: Scopes,
    params: HashMap<Name, ParamIdx>,
}

impl BodyLower {
    pub(super) fn new(params: HashMap<Name, ParamIdx>) -> Self {
        Self {
            scopes: Scopes::new(),
            params,
        }
    }

    pub(super) fn lower_toplevel(
        &mut self,
        ast: ast::Stmt,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Option<Stmt> {
        match ast {
            ast::Stmt::FunctionDef(def) => self.lower_function(def, ctx, db, item_tree, interner),
            _ => None,
        }
    }

    fn lower_function(
        &mut self,
        def: ast::FunctionDef,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Option<Stmt> {
        let body = def.body()?;
        let body_ast_id = db.lookup_ast_id(&body).unwrap();
        let function = item_tree.function_by_block(db, &body_ast_id).unwrap();
        let function_idx = item_tree.function_idx_by_block(&body_ast_id).unwrap();

        let mut body_lower = BodyLower::new(function.param_by_name.clone());
        let expr = body_lower.lower_block(body, ctx, db, item_tree, interner);
        let body_idx = ctx.function_bodies.alloc(expr);
        ctx.function_body_by_block.insert(body_ast_id, body_idx);

        Some(Stmt::FunctionDef {
            signature: function_idx,
            body: body_idx,
        })
    }

    fn lower_stmt(
        &mut self,
        ast: ast::Stmt,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Option<Stmt> {
        let result = match ast {
            ast::Stmt::VariableDef(def) => {
                let expr = self.lower_expr(def.value(), ctx, db, item_tree, interner);
                let idx = ctx.exprs.alloc(expr);
                let name = Name::from_key(interner.intern(def.name()?.name()));
                self.scopes.define(name, idx);
                Stmt::VariableDef { name, value: idx }
            }
            ast::Stmt::Expr(ast) => {
                let expr = self.lower_expr(Some(ast), ctx, db, item_tree, interner);
                Stmt::Expr(ctx.exprs.alloc(expr))
            }
            ast::Stmt::FunctionDef(def) => {
                self.lower_function(def, ctx, db, item_tree, interner)?
            }
        };

        Some(result)
    }

    fn lower_expr(
        &mut self,
        ast: Option<ast::Expr>,
        ctx: &mut SharedBodyLowerContext,
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
                ast::Expr::Call(ast) => self.lower_call(ast, ctx, db, item_tree, interner),
                ast::Expr::Block(ast) => self.lower_block(ast, ctx, db, item_tree, interner),
                ast::Expr::IfExpr(ast) => self.lower_if(ast, ctx, db, item_tree, interner),
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
        ctx: &mut SharedBodyLowerContext,
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
            lhs: ctx.exprs.alloc(lhs),
            rhs: ctx.exprs.alloc(rhs),
        }
    }

    fn lower_unary(
        &mut self,
        ast: ast::UnaryExpr,
        ctx: &mut SharedBodyLowerContext,
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
            expr: ctx.exprs.alloc(expr),
        }
    }

    fn lower_variable_ref(
        &mut self,
        ast: ast::VariableRef,
        _ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Expr {
        let ident = ast.name().unwrap();
        let symbol = self.lookup_ident(&ident, _ctx, db, item_tree, interner);

        Expr::VariableRef { var: symbol }
    }

    fn lookup_ident(
        &mut self,
        ast: &ast::Ident,
        _ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Symbol {
        let name = Name::from_key(interner.intern(ast.name()));
        if let Some(expr) = self.scopes.lookup_in_only_current_scope(name) {
            Symbol::Local { name, expr }
        } else if let Some(param_idx) = self.lookup_param(name) {
            Symbol::Param {
                name,
                param: param_idx,
            }
        } else {
            let item_scope = match self.scopes.current_scope() {
                ScopeType::TopLevel => item_tree.top_level_scope(db),
                ScopeType::SubLevel(block_ast_id) => {
                    item_tree.scope_by_block(db, block_ast_id).unwrap()
                }
            };
            if let Some(function) = item_scope.lookup(ast.name(), db, item_tree, interner) {
                Symbol::Function { name, function }
            } else if let Some(expr) = self.scopes.lookup(name) {
                Symbol::Local { name, expr }
            } else {
                Symbol::Missing { name }
            }
        }
    }

    fn lookup_param(&self, name: Name) -> Option<ParamIdx> {
        self.params.get(&name).copied()
    }

    fn lower_call(
        &mut self,
        ast: ast::Call,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Expr {
        let args = ast.args().unwrap();
        let args = args
            .args()
            .map(|arg| {
                let expr = self.lower_expr(arg.expr(), ctx, db, item_tree, interner);
                ctx.exprs.alloc(expr)
            })
            .collect();
        let name = ast.callee().unwrap();
        let callee = self.lookup_ident(&name, ctx, db, item_tree, interner);

        Expr::Call { callee, args }
    }

    fn lower_block(
        &mut self,
        ast: ast::Block,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Expr {
        let block_ast_id = if let Some(ast_id) = db.lookup_ast_id(&ast) {
            ast_id
        } else {
            return Expr::Missing;
        };
        self.scopes.enter(block_ast_id.clone());

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
            ast: block_ast_id,
        })
    }

    fn lower_if(
        &mut self,
        ast: ast::IfExpr,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
        interner: &mut Interner,
    ) -> Expr {
        let condition = self.lower_expr(ast.condition(), ctx, db, item_tree, interner);
        let condition = ctx.exprs.alloc(condition);

        let then_branch =
            self.lower_block(ast.then_branch().unwrap(), ctx, db, item_tree, interner);
        let then_branch = ctx.exprs.alloc(then_branch);

        let else_branch = if let Some(else_branch) = ast.else_branch() {
            let else_branch = self.lower_block(else_branch, ctx, db, item_tree, interner);
            Some(ctx.exprs.alloc(else_branch))
        } else {
            None
        };

        Expr::If {
            condition,
            then_branch,
            else_branch,
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::AstNode;
    use expect_test::{expect, Expect};

    use super::*;
    use crate::{
        item_tree::{Function, Type},
        lower, LowerError, LowerResult,
    };

    fn indent(nesting: usize) -> String {
        "    ".repeat(nesting)
    }

    fn debug(lower_result: &LowerResult) -> String {
        let mut msg = "".to_string();

        for stmt in &lower_result.top_level_stmts {
            msg.push_str(&debug_stmt(lower_result, stmt, 0));
        }

        for error in &lower_result.errors {
            match error {
                LowerError::UndefinedEntryPoint => {
                    msg.push_str("error: Undefined entry point.(help: fn main() { ... })\n");
                }
            }
        }

        msg
    }

    fn debug_function(
        lower_result: &LowerResult,
        function_idx: Idx<Function>,
        nesting: usize,
    ) -> String {
        let function = &lower_result.db.functions[function_idx];
        let block_ast_id = lower_result
            .item_tree
            .block_idx_by_function(&function_idx)
            .unwrap();
        let body_expr = lower_result
            .shared_ctx
            .function_body_by_block(&block_ast_id)
            .unwrap();

        let name = if let Some(name) = function.name {
            lower_result.interner.lookup(name.key())
        } else {
            "<missing>"
        };
        let params = function
            .params
            .iter()
            .map(|param| {
                let param = &lower_result.db.params[*param];
                let name = if let Some(name) = param.name {
                    lower_result.interner.lookup(name.key())
                } else {
                    "<missing>"
                };
                let ty = match param.ty {
                    Type::Integer => "int",
                    Type::String => "string",
                    Type::Char => "char",
                    Type::Boolean => "bool",
                    Type::Unit => "()",
                    Type::Unknown => "<unknown>",
                };
                format!("{name}: {ty}")
            })
            .collect::<Vec<_>>()
            .join(", ");
        let return_type = match &function.return_type {
            Type::Integer => "int",
            Type::String => "string",
            Type::Char => "char",
            Type::Boolean => "bool",
            Type::Unit => "()",
            Type::Unknown => "<unknown>",
        };

        let body = debug_expr(lower_result, body_expr, nesting);
        let is_entry_point = lower_result.entry_point == Some(function_idx);
        format!(
            "{}fn {}{name}({params}) -> {return_type} {body}\n",
            indent(nesting),
            if is_entry_point { "entry:" } else { "" }
        )
    }

    fn debug_stmt(lower_result: &LowerResult, stmt: &Stmt, nesting: usize) -> String {
        match stmt {
            Stmt::VariableDef { name, value } => {
                let name = lower_result.interner.lookup(name.key());
                let expr_str = debug_expr(
                    lower_result,
                    &lower_result.shared_ctx.exprs[*value],
                    nesting,
                );
                format!("{}let {} = {}\n", indent(nesting), name, expr_str)
            }
            Stmt::Expr(expr) => format!(
                "{}{}\n",
                indent(nesting),
                debug_expr(lower_result, &lower_result.shared_ctx.exprs[*expr], nesting)
            ),
            Stmt::FunctionDef { body, .. } => {
                let body = &lower_result.shared_ctx.function_bodies[*body];
                if let Expr::Block(block) = body {
                    let function_idx = lower_result
                        .item_tree
                        .function_idx_by_block(&block.ast)
                        .unwrap();
                    debug_function(lower_result, function_idx, nesting)
                } else {
                    panic!("supported only block");
                }
            }
        }
    }

    fn debug_expr(lower_result: &LowerResult, expr: &Expr, nesting: usize) -> String {
        match expr {
            Expr::Literal(literal) => match literal {
                Literal::Bool(b) => b.to_string(),
                Literal::Char(c) => format!("'{c}'"),
                Literal::String(s) => format!("\"{s}\""),
                Literal::Integer(i) => i.to_string(),
            },
            Expr::Binary { op, lhs, rhs } => {
                let op = match op {
                    BinaryOp::Add => "+",
                    BinaryOp::Sub => "-",
                    BinaryOp::Mul => "*",
                    BinaryOp::Div => "/",
                };
                let lhs_str =
                    debug_expr(lower_result, &lower_result.shared_ctx.exprs[*lhs], nesting);
                let rhs_str =
                    debug_expr(lower_result, &lower_result.shared_ctx.exprs[*rhs], nesting);
                format!("{lhs_str} {op} {rhs_str}")
            }
            Expr::Unary { op, expr } => {
                let op = match op {
                    UnaryOp::Neg => "-",
                };
                let expr_str =
                    debug_expr(lower_result, &lower_result.shared_ctx.exprs[*expr], nesting);
                format!("{op}{expr_str}")
            }
            Expr::VariableRef { var } => debug_symbol(lower_result, var, nesting),
            Expr::Call { callee, args } => {
                let callee = debug_symbol(lower_result, callee, nesting);
                let args = args
                    .iter()
                    .map(|arg| {
                        debug_expr(lower_result, &lower_result.shared_ctx.exprs[*arg], nesting)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{callee}({args})")
            }
            Expr::Block(block) => {
                let mut msg = "{\n".to_string();
                for stmt in &block.stmts {
                    msg.push_str(&debug_stmt(lower_result, stmt, nesting + 1));
                }
                if let Some(tail) = block.tail {
                    msg.push_str(&format!(
                        "{}expr:{}\n",
                        indent(nesting + 1),
                        debug_expr(
                            lower_result,
                            &lower_result.shared_ctx.exprs[tail],
                            nesting + 1
                        )
                    ));
                }
                msg.push_str(&format!("{}}}", indent(nesting)));

                msg
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut msg = "if ".to_string();
                msg.push_str(&debug_expr(
                    lower_result,
                    &lower_result.shared_ctx.exprs[*condition],
                    nesting,
                ));
                msg.push(' ');
                msg.push_str(&debug_expr(
                    lower_result,
                    &lower_result.shared_ctx.exprs[*then_branch],
                    nesting,
                ));

                if let Some(else_branch) = else_branch {
                    msg.push_str(" else ");
                    msg.push_str(&debug_expr(
                        lower_result,
                        &lower_result.shared_ctx.exprs[*else_branch],
                        nesting,
                    ));
                }

                msg
            }
            Expr::Missing => "<missing>".to_string(),
        }
    }

    fn debug_symbol(lower_result: &LowerResult, symbol: &Symbol, nesting: usize) -> String {
        match symbol {
            Symbol::Local { name, expr } => match lower_result.shared_ctx.exprs[*expr] {
                Expr::Binary { .. }
                | Expr::Missing
                | Expr::Literal(_)
                | Expr::Unary { .. }
                | Expr::VariableRef { .. }
                | Expr::Call { .. }
                | Expr::If { .. } => {
                    debug_expr(lower_result, &lower_result.shared_ctx.exprs[*expr], nesting)
                }
                Expr::Block { .. } => lower_result.interner.lookup(name.key()).to_string(),
            },
            Symbol::Param { name, .. } => {
                let name = lower_result.interner.lookup(name.key());
                format!("param:{name}")
            }
            Symbol::Function { name, .. } => {
                let name = lower_result.interner.lookup(name.key());
                format!("fn:{name}")
            }
            Symbol::Missing { .. } => "<missing>".to_string(),
        }
    }

    fn parse(input: &str) -> ast::SourceFile {
        ast::SourceFile::cast(parser::parse(input).syntax()).unwrap()
    }

    fn check(input: &str, expected: Expect) {
        let source_file = parse(input);
        let result = lower(source_file);

        expected.assert_eq(&debug(&result));
    }

    #[test]
    fn entry_point() {
        check(
            r#"
                fn no_main() { }
            "#,
            expect![[r#"
                fn no_main() -> () {
                }
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
        check(
            r#"
                fn main() { }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                }
            "#]],
        );
    }

    #[test]
    fn lower_variable_def() {
        check(
            r#"
                fn main() {
                    let foo = bar
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    let foo = <missing>
                }
            "#]],
        );
    }

    #[test]
    fn lower_variable_def_without_name() {
        check(
            r#"
                fn main() {
                    let = 10
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                }
            "#]],
        );
    }

    #[test]
    fn lower_variable_def_without_eq() {
        check(
            r#"
                fn main() {
                    let foo 10
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    let foo = <missing>
                }
            "#]],
        );
    }

    #[test]
    fn lower_variable_def_without_value() {
        check(
            r#"
                fn main() {
                    let a =
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    let a = <missing>
                }
            "#]],
        );
    }

    #[test]
    fn lower_expr_stmt() {
        check(
            r#"
                fn main() {
                    123
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:123
                }
            "#]],
        );
    }

    #[test]
    fn lower_binary_expr() {
        check(
            r#"
                fn main() {
                    1 + 2
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:1 + 2
                }
            "#]],
        );
    }

    #[test]
    fn lower_binary_expr_without_rhs() {
        check(
            r#"
                fn main() {
                    10 -
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:10 - <missing>
                }
            "#]],
        );
    }

    #[test]
    fn lower_integer_literal() {
        check(
            r#"
                fn main() {
                    999
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:999
                }
            "#]],
        );
    }

    #[test]
    fn lower_string_literal() {
        check(
            r#"
                fn main() {
                    "aaa"
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:"aaa"
                }
            "#]],
        );
    }

    #[test]
    fn lower_char_literal() {
        check(
            r#"
                fn main() {
                    'a'
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:'a'
                }
            "#]],
        );
    }

    #[test]
    fn lower_bool_literal_true() {
        check(
            r#"
                fn main() {
                    true
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:true
                }
            "#]],
        );
    }

    #[test]
    fn lower_bool_literal_false() {
        check(
            r#"
                fn main() {
                    false
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:false
                }
            "#]],
        );
    }

    #[test]
    fn lower_paren_expr() {
        check(
            r#"
                fn main() {
                    ((((((abc))))))
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:<missing>
                }
            "#]],
        );
    }

    #[test]
    fn lower_unary_expr() {
        check(
            r#"
                fn main() {
                    -10
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:-10
                }
            "#]],
        );
    }

    #[test]
    fn lower_unary_expr_without_expr() {
        check(
            r#"
                fn main() {
                    -
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:-<missing>
                }
            "#]],
        );
    }

    #[test]
    fn lower_variable_ref() {
        check(
            r#"
                fn main() {
                    foo
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:<missing>
                }
            "#]],
        );
    }

    #[test]
    fn lookup_variable_ref() {
        check(
            r#"
                fn main() {
                    let foo = 10
                    foo
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    let foo = 10
                    expr:10
                }
            "#]],
        );
    }

    #[test]
    fn lower_block() {
        check(
            r#"
                fn main() {
                    {
                        let foo = 10;
                    }
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:{
                        let foo = 10
                    }
                }
            "#]],
        );
    }

    #[test]
    fn lower_nested_block() {
        check(
            r#"
                fn main() {
                    {
                        let a = 10
                        {
                            let b = 20
                            let c = 30
                        }
                        let d = 40
                    }
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:{
                        let a = 10
                        {
                            let b = 20
                            let c = 30
                        }
                        let d = 40
                    }
                }
            "#]],
        );
    }

    #[test]
    fn lower_with_scope() {
        check(
            r#"
                fn main() {
                    {
                        let a = 10
                    }
                    a
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    {
                        let a = 10
                    }
                    expr:<missing>
                }
            "#]],
        );
    }

    #[test]
    fn lower_with_nested_scope() {
        check(
            r#"
                fn main() {
                    let a = 10
                    {
                        let b = 20
                        {
                            let c = 30
                            a + b + c
                        }
                    }
                    a
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    let a = 10
                    {
                        let b = 20
                        expr:{
                            let c = 30
                            expr:10 + 20 + 30
                        }
                    }
                    expr:10
                }
            "#]],
        );
    }

    #[test]
    fn shadwing() {
        check(
            r#"
                fn main() {
                    let a = 10
                    a
                    let a = 20
                    a
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    let a = 10
                    10
                    let a = 20
                    expr:20
                }
            "#]],
        );
    }

    #[test]
    fn shadwing_on_block() {
        check(
            r#"
                fn main() {
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
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
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
                    expr:10
                }
            "#]],
        );
    }

    #[test]
    fn can_bind_block_because_it_is_expr() {
        check(
            r#"
                fn main() {
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
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    let a = {
                        <missing>
                        let a = 10
                        expr:10
                    }
                    a
                    expr:20 + {
                        let b = 30
                        expr:a + 30
                    }
                }
            "#]],
        );
    }

    #[test]
    fn can_use_block_in_binary_expr() {
        check(
            r#"
                fn main() {
                    {
                        let a = 10;
                        a
                    } + {
                        let b = 30
                        a + b
                    }
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:{
                        let a = 10
                        expr:10
                    } + {
                        let b = 30
                        expr:<missing> + 30
                    }
                }
            "#]],
        );
    }

    #[test]
    fn can_use_block_in_paren_expr() {
        check(
            r#"
                fn main() {
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
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:{
                        let a = 10
                        expr:{
                            expr:10
                        }
                    }
                }
            "#]],
        );
    }

    #[test]
    fn can_use_block_in_unary_expr() {
        check(
            r#"
                fn main() {
                    -{
                        let a = 10;
                        -{
                            -a
                        }
                    }
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:-{
                        let a = 10
                        expr:-{
                            expr:-10
                        }
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
                fn foo() -> () {
                    let a = 10
                    expr:10
                }
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
    }

    #[test]
    fn define_function_with_params() {
        check(
            r#"
                fn foo(a: int, b: string) {
                    1
                }
            "#,
            expect![[r#"
                fn foo(a: int, b: string) -> () {
                    expr:1
                }
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
    }

    #[test]
    fn define_function_with_return_type() {
        check(
            r#"
                fn foo() -> int {}
                fn bar() -> string {}
                fn bar() -> char {}
                fn bar() -> bool {}
            "#,
            expect![[r#"
                fn foo() -> int {
                }
                fn bar() -> string {
                }
                fn bar() -> char {
                }
                fn bar() -> bool {
                }
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
    }

    #[test]
    fn resolve_function_params() {
        check(
            r#"
                fn foo(a: char, b: bool) {
                    a + b
                }
            "#,
            expect![[r#"
                fn foo(a: char, b: bool) -> () {
                    expr:param:a + param:b
                }
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
    }

    #[test]
    fn resolve_function_params_by_nested_scope() {
        check(
            r#"
                fn main() {
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
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    fn foo(a: <unknown>, b: <unknown>) -> () {
                        let a = 0
                        0
                        param:b
                        fn bar(a: <unknown>, b: <unknown>) -> () {
                            expr:{
                                expr:param:a + param:b
                            }
                        }
                        0
                        expr:param:b
                    }
                    <missing>
                    expr:<missing>
                }
            "#]],
        );
    }

    #[test]
    fn function_scope() {
        check(
            r#"
                fn main() {
                    let a = 10;
                    fn foo() {
                        a
                        let a = 20;
                        a
                    }
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    let a = 10
                    fn foo() -> () {
                        <missing>
                        let a = 20
                        expr:20
                    }
                }
            "#]],
        );
    }

    #[test]
    fn define_function_in_block() {
        check(
            r#"
                fn main() {
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
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:{
                        fn foo() -> () {
                            let a = 10
                            expr:{
                                let b = 20
                                fn bar() -> () {
                                    let c = 20
                                    fn baz() -> () {
                                        expr:<missing> + <missing> + <missing>
                                    }
                                    expr:<missing> + <missing> + 20
                                }
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
                fn main() {
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
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    <missing>
                    let a = 10
                    10
                    fn foo() -> () {
                        <missing>
                        let a = 20
                        20
                        {
                            20
                            let a = 30
                            expr:30
                        }
                        20
                        fn bar() -> () {
                            <missing>
                            let a = 40
                            expr:40
                        }
                        expr:20
                    }
                    expr:10
                }
            "#]],
        );
    }

    #[test]
    fn ref_function_from_outer_scope() {
        check(
            r#"
                fn main() {
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
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    fn foo() -> () {
                        fn:foo
                        fn:bar
                        fn:baz
                        fn bar() -> () {
                            fn:foo
                            fn:bar
                            expr:fn:baz
                        }
                    }
                    fn baz() -> () {
                        fn:foo
                        <missing>
                        expr:fn:baz
                    }
                    fn:foo
                    <missing>
                    expr:fn:baz
                }
            "#]],
        );
    }

    #[test]
    fn unclose_block_in_expr() {
        check(
            "fn main() { ($10/{ }",
            expect![[r#"
                fn entry:main() -> () {
                    expr:<missing> / <missing>
                }
            "#]],
        );
    }

    #[test]
    fn function_missing_param() {
        check(
            "fn a(a, ) {}",
            expect![[r#"
                fn a(a: <unknown>, <missing>: <unknown>) -> () {
                }
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
    }

    #[test]
    fn function_missing_return_type() {
        check(
            "fn a() -> {}",
            expect![[r#"
                fn a() -> <unknown> {
                }
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
    }

    #[test]
    fn function_missing_block() {
        check(
            "fn a(a, )",
            expect![[r#"
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
    }

    #[test]
    fn function_missing_name() {
        check(
            "fn (a, ) {}",
            expect![[r#"
                fn <missing>(a: <unknown>, <missing>: <unknown>) -> () {
                }
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
    }

    #[test]
    fn call() {
        check(
            "fn main() { a() }",
            expect![[r#"
                fn entry:main() -> () {
                    expr:<missing>()
                }
            "#]],
        );

        check(
            r#"
                fn main() {
                    fn a() { 10 }
                    a()
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    fn a() -> () {
                        expr:10
                    }
                    expr:fn:a()
                }
            "#]],
        );
    }

    #[test]
    fn call_with_arg() {
        check(
            "fn main() { a(10, 20) }",
            expect![[r#"
                fn entry:main() -> () {
                    expr:<missing>(10, 20)
                }
            "#]],
        );

        check(
            r#"
                fn main() {
                    fn a() { 10 }
                    let b = 20;
                    a(b, 30)
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    fn a() -> () {
                        expr:10
                    }
                    let b = 20
                    expr:fn:a(20, 30)
                }
            "#]],
        );

        check(
            r#"
                fn main() {
                    fn aaa(x: bool, y: string) -> int {
                        10 + 20
                    }
                    aaa("aaa", true);
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    fn aaa(x: bool, y: string) -> int {
                        expr:10 + 20
                    }
                    expr:fn:aaa("aaa", true)
                }
            "#]],
        );
    }

    #[test]
    fn if_expr() {
        check(
            r#"
                fn main() {
                    if true {
                        10
                    } else {
                        20
                    }
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:if true <missing> else <missing>
                }
            "#]],
        );
    }
}
