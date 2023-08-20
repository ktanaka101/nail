mod scopes;

use std::collections::HashMap;

use la_arena::{Arena, Idx};

use self::scopes::ScopeType;
use crate::{
    body::scopes::ExprScopes, db::Database, item_tree::ItemTree, AstId, Block, Expr, FileId,
    FunctionId, ItemDefId, Literal, ModuleKind, Name, ParamId, Path, Stmt, Symbol,
};

/// 式を一意に識別するためのID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(Idx<Expr>);
impl ExprId {
    /// このIDに対応する式を取得する
    pub fn lookup(self, ctx: &SharedBodyLowerContext) -> &Expr {
        &ctx.exprs[self.0]
    }
}
impl PartialOrd for ExprId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.into_raw().partial_cmp(&other.0.into_raw())
    }
}
impl Ord for ExprId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.into_raw().cmp(&other.0.into_raw())
    }
}

/// 関数本体を一意に識別するためのID
///
/// TODO: [Expr]の代わりに`Block`等を使うようにし、構造を明確にする
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionBodyId(Idx<Expr>);

/// 1ファイル内における式と関数本体を保持する
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SharedBodyLowerContext {
    function_bodies: Arena<Expr>,
    exprs: Arena<Expr>,
    function_body_by_block: HashMap<AstId<ast::BlockExpr>, FunctionBodyId>,
}
impl SharedBodyLowerContext {
    /// 空のコンテキストを作成する
    pub fn new() -> Self {
        Self {
            function_bodies: Arena::new(),
            exprs: Arena::new(),
            function_body_by_block: HashMap::new(),
        }
    }

    /// ASTブロックIDを元に関数本体IDを取得する
    pub fn function_body_id_by_block(
        &self,
        block_ast_id: AstId<ast::BlockExpr>,
    ) -> Option<FunctionBodyId> {
        self.function_body_by_block.get(&block_ast_id).copied()
    }

    /// ASTブロックIDを元に関数本体を取得する
    ///
    /// 現状、関数本体は[Expr::Block]として表現されているため、[Expr]を返す
    pub fn function_body_by_block(&self, block_ast_id: AstId<ast::BlockExpr>) -> Option<&Expr> {
        self.function_body_id_by_block(block_ast_id)
            .map(|body_id| &self.function_bodies[body_id.0])
    }

    /// 式を保存し、参照するためのIDを返す
    pub(crate) fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        ExprId(self.exprs.alloc(expr))
    }

    /// 関数本体を保存し、参照するためのIDを返す
    pub(crate) fn alloc_function_body(&mut self, expr: Expr) -> FunctionBodyId {
        FunctionBodyId(self.function_bodies.alloc(expr))
    }
}

/// ASTを元にHIRを構築するためのコンテキスト/ビルダー
///
/// ユースケースはトップレベルと関数本体の2種類あります。
/// トップレベルの場合、`params`は空で指定します。
/// 関数本体の場合、`params`に関数パラメータを指定します。
/// `params`は、名前解決に使用されます。
#[derive(Debug)]
pub struct BodyLower {
    file_id: FileId,
    scopes: ExprScopes,
    params: HashMap<Name, ParamId>,
}

impl BodyLower {
    /// 空のコンテキストを作成する
    pub(super) fn new(file_id: FileId, params: HashMap<Name, ParamId>) -> Self {
        Self {
            file_id,
            scopes: ExprScopes::new(),
            params,
        }
    }

    /// トップレベルに定義された[ast::Item]を元に、トップレベルのHIRを構築する
    pub(super) fn lower_toplevel(
        &mut self,
        salsa_db: &dyn crate::Db,
        ast: ast::Item,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Option<ItemDefId> {
        match ast {
            ast::Item::FunctionDef(def) => self.lower_function(salsa_db, def, ctx, db, item_tree),
            ast::Item::Module(module) => self.lower_module(salsa_db, module, ctx, db, item_tree),
            ast::Item::Use(r#use) => self.lower_use(r#use, db, item_tree),
        }
    }

    fn lower_function(
        &mut self,
        salsa_db: &dyn crate::Db,
        def: ast::FunctionDef,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Option<ItemDefId> {
        let body = def.body()?;
        let body_ast_id = db.lookup_ast_id(&body, self.file_id).unwrap();
        let function = item_tree.function_by_block(db, &body_ast_id).unwrap();
        let function_id = item_tree.function_id_by_block(&body_ast_id).unwrap();

        let mut body_lower = BodyLower::new(self.file_id, function.param_by_name.clone());
        let body_expr = body_lower.lower_block(salsa_db, body, ctx, db, item_tree);
        let body_id = ctx.alloc_function_body(body_expr);
        ctx.function_body_by_block.insert(body_ast_id, body_id);

        Some(ItemDefId::Function(function_id))
    }

    /// モジュールのHIRを構築します。
    ///
    /// アウトラインモジュール(`mod aaa;`)のパースは[crate::parse_modules]で行います。
    /// この中では、トラバースするために各アイテムのHIR化を行っています。
    /// また、[ItemDefId]を返すために、アウトラインモジュールも返します。
    fn lower_module(
        &mut self,
        salsa_db: &dyn crate::Db,
        module: ast::Module,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Option<ItemDefId> {
        let module_ast_id = db.lookup_ast_id(&module, self.file_id).unwrap();
        let module_id = item_tree
            .module_id_by_ast_module(module_ast_id.clone())
            .unwrap();

        let hir_module = item_tree
            .module_by_ast_module_id(db, module_ast_id)
            .unwrap();
        match &hir_module.kind {
            ModuleKind::Inline { .. } => {
                for item in module.items().expect("Already parsed ").items() {
                    self.lower_item(salsa_db, item, ctx, db, item_tree);
                }

                Some(ItemDefId::Module(module_id))
            }
            ModuleKind::Outline => Some(ItemDefId::Module(module_id)),
        }
    }

    fn lower_use(
        &mut self,
        r#use: ast::Use,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Option<ItemDefId> {
        let use_ast_id = db.lookup_ast_id(&r#use, self.file_id).unwrap();
        let use_item_id = item_tree.use_item_id_by_ast_use(use_ast_id).unwrap();

        Some(ItemDefId::UseItem(use_item_id))
    }

    fn lower_stmt(
        &mut self,
        salsa_db: &dyn crate::Db,
        ast: ast::Stmt,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Option<Stmt> {
        let result = match ast {
            ast::Stmt::VariableDef(def) => {
                let expr = self.lower_expr(salsa_db, def.value(), ctx, db, item_tree);
                let id = ctx.alloc_expr(expr);
                let name = Name::new(salsa_db, def.name()?.name().to_owned());
                self.scopes.define(name, id);
                Stmt::VariableDef { name, value: id }
            }
            ast::Stmt::ExprStmt(ast) => {
                let expr = self.lower_expr(salsa_db, ast.expr(), ctx, db, item_tree);
                Stmt::ExprStmt {
                    expr: ctx.alloc_expr(expr),
                    has_semicolon: ast.semicolon().is_some(),
                }
            }
            ast::Stmt::Item(item) => {
                let item = self.lower_item(salsa_db, item, ctx, db, item_tree)?;
                Stmt::Item { item }
            }
        };

        Some(result)
    }

    fn lower_item(
        &mut self,
        salsa_db: &dyn crate::Db,
        item: ast::Item,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Option<ItemDefId> {
        match item {
            ast::Item::FunctionDef(def) => self.lower_function(salsa_db, def, ctx, db, item_tree),
            ast::Item::Module(module) => self.lower_module(salsa_db, module, ctx, db, item_tree),
            ast::Item::Use(r#use) => self.lower_use(r#use, db, item_tree),
        }
    }

    fn lower_expr(
        &mut self,
        salsa_db: &dyn crate::Db,
        ast: Option<ast::Expr>,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Expr {
        if let Some(ast) = ast {
            match ast {
                ast::Expr::BinaryExpr(ast) => self.lower_binary(salsa_db, ast, ctx, db, item_tree),
                ast::Expr::Literal(ast) => self.lower_literal(ast),
                ast::Expr::ParenExpr(ast) => {
                    self.lower_expr(salsa_db, ast.expr(), ctx, db, item_tree)
                }
                ast::Expr::UnaryExpr(ast) => self.lower_unary(salsa_db, ast, ctx, db, item_tree),
                ast::Expr::PathExpr(ast) => self.lower_path_expr(salsa_db, ast, ctx, db, item_tree),
                ast::Expr::CallExpr(ast) => self.lower_call(salsa_db, ast, ctx, db, item_tree),
                ast::Expr::BlockExpr(ast) => self.lower_block(salsa_db, ast, ctx, db, item_tree),
                ast::Expr::IfExpr(ast) => self.lower_if(salsa_db, ast, ctx, db, item_tree),
                ast::Expr::ReturnExpr(ast) => self.lower_return(salsa_db, ast, ctx, db, item_tree),
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
        salsa_db: &dyn crate::Db,
        ast: ast::BinaryExpr,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Expr {
        let op = ast.op().unwrap();

        let lhs = self.lower_expr(salsa_db, ast.lhs(), ctx, db, item_tree);
        let rhs = self.lower_expr(salsa_db, ast.rhs(), ctx, db, item_tree);

        Expr::Binary {
            op,
            lhs: ctx.alloc_expr(lhs),
            rhs: ctx.alloc_expr(rhs),
        }
    }

    fn lower_unary(
        &mut self,
        salsa_db: &dyn crate::Db,
        ast: ast::UnaryExpr,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Expr {
        let op = ast.op().unwrap();

        let expr = self.lower_expr(salsa_db, ast.expr(), ctx, db, item_tree);

        Expr::Unary {
            op,
            expr: ctx.alloc_expr(expr),
        }
    }

    fn lower_path_expr(
        &mut self,
        salsa_db: &dyn crate::Db,
        ast: ast::PathExpr,
        _ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Expr {
        let path = Path {
            segments: ast
                .path()
                .unwrap()
                .segments()
                .map(|segment| Name::new(salsa_db, segment.name().unwrap().name().to_string()))
                .collect(),
        };
        let symbol = self.lookup_path(path, _ctx, db, item_tree);
        Expr::Symbol(symbol)
    }

    fn lookup_path(
        &mut self,
        path: Path,
        _ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Symbol {
        match path.segments() {
            [name] => {
                let name = *name;
                if let Some(expr) = self.scopes.lookup_in_only_current_scope(name) {
                    Symbol::Local { name, expr }
                } else if let Some(param_id) = self.lookup_param(name) {
                    Symbol::Param {
                        name,
                        param: param_id,
                    }
                } else if let Some(function) = self.lookup_function(&[], name, db, item_tree) {
                    Symbol::Function {
                        path: Path {
                            segments: vec![name],
                        },
                        function,
                    }
                } else if let Some(expr) = self.scopes.lookup(name) {
                    Symbol::Local { name, expr }
                } else {
                    Symbol::Missing {
                        path: Path {
                            segments: vec![name],
                        },
                    }
                }
            }
            [segments @ .., item_segment] => {
                if let Some(function) = self.lookup_function(segments, *item_segment, db, item_tree)
                {
                    Symbol::Function {
                        path: Path {
                            segments: path.segments().to_vec(),
                        },
                        function,
                    }
                } else {
                    Symbol::Missing {
                        path: Path {
                            segments: path.segments().to_vec(),
                        },
                    }
                }
            }
            [] => unreachable!(),
        }
    }

    fn lookup_function(
        &self,
        module_paths: &[Name],
        function_name: Name,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Option<FunctionId> {
        let item_scope = match self.scopes.current_scope() {
            ScopeType::TopLevel => item_tree.top_level_scope(db),
            ScopeType::SubLevel(block_ast_id) => {
                item_tree.scope_by_block(db, block_ast_id).unwrap()
            }
        };
        item_scope.lookup(module_paths, function_name, db, item_tree)
    }

    fn lookup_param(&self, name: Name) -> Option<ParamId> {
        self.params.get(&name).copied()
    }

    fn lower_call(
        &mut self,
        salsa_db: &dyn crate::Db,
        ast: ast::CallExpr,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Expr {
        let args = ast.args().unwrap();
        let args = args
            .args()
            .map(|arg| {
                let expr = self.lower_expr(salsa_db, arg.expr(), ctx, db, item_tree);
                ctx.alloc_expr(expr)
            })
            .collect();
        let callee = match self.lower_expr(salsa_db, ast.callee(), ctx, db, item_tree) {
            Expr::Symbol(symbol) => symbol,
            Expr::Binary { .. } => todo!(),
            Expr::Literal(_) => todo!(),
            Expr::Unary { .. } => todo!(),
            Expr::Call { .. } => todo!(),
            Expr::Block(_) => todo!(),
            Expr::If { .. } => todo!(),
            Expr::Return { .. } => todo!(),
            Expr::Missing => todo!(),
        };

        Expr::Call { callee, args }
    }

    fn lower_block(
        &mut self,
        salsa_db: &dyn crate::Db,
        ast: ast::BlockExpr,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Expr {
        let block_ast_id = if let Some(ast_id) = db.lookup_ast_id(&ast, self.file_id) {
            ast_id
        } else {
            return Expr::Missing;
        };
        self.scopes.enter(block_ast_id.clone());

        let mut stmts = vec![];
        for stmt in ast.stmts() {
            if let Some(stmt) = self.lower_stmt(salsa_db, stmt, ctx, db, item_tree) {
                stmts.push(stmt);
            }
        }

        self.scopes.leave();

        let tail = match stmts.last() {
            Some(Stmt::ExprStmt {
                expr,
                has_semicolon,
            }) if !has_semicolon => {
                let expr = *expr;
                stmts.pop();
                Some(expr)
            }
            _ => None,
        };

        Expr::Block(Block {
            stmts,
            tail,
            ast: block_ast_id,
        })
    }

    fn lower_if(
        &mut self,
        salsa_db: &dyn crate::Db,
        ast: ast::IfExpr,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Expr {
        let condition = self.lower_expr(salsa_db, ast.condition(), ctx, db, item_tree);
        let condition = ctx.alloc_expr(condition);

        let then_branch =
            self.lower_block(salsa_db, ast.then_branch().unwrap(), ctx, db, item_tree);
        let then_branch = ctx.alloc_expr(then_branch);

        let else_branch = if let Some(else_branch) = ast.else_branch() {
            let else_branch = self.lower_block(salsa_db, else_branch, ctx, db, item_tree);
            Some(ctx.alloc_expr(else_branch))
        } else {
            None
        };

        Expr::If {
            condition,
            then_branch,
            else_branch,
        }
    }

    fn lower_return(
        &mut self,
        salsa_db: &dyn crate::Db,
        ast: ast::ReturnExpr,
        ctx: &mut SharedBodyLowerContext,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Expr {
        let value = if let Some(value) = ast.value() {
            let value = self.lower_expr(salsa_db, Some(value), ctx, db, item_tree);
            Some(ctx.alloc_expr(value))
        } else {
            None
        };

        Expr::Return { value }
    }
}

#[cfg(test)]
mod tests {
    use ast::AstNode;
    use expect_test::{expect, Expect};

    use super::*;
    use crate::{
        db::UseItemId,
        input::{FixtureDatabase, SourceDatabaseTrait},
        item_tree::{ItemDefId, Type},
        lower_root, parse_pod, FunctionId, LowerError, LowerResult, ModuleId, ModuleKind, Pod,
        SalsaDatabase,
    };

    fn indent(nesting: usize) -> String {
        "    ".repeat(nesting)
    }

    fn debug_modules(
        salsa_db: &dyn crate::Db,
        modules: &Pod,
        source_db: &FixtureDatabase,
    ) -> String {
        let mut msg = "".to_string();

        msg.push_str("//- /main.nail\n");
        msg.push_str(&debug_lower_result(salsa_db, &modules.root_lower_result));

        for (file_id, lower_result) in modules.lower_results_order_registration_asc() {
            let file_path = source_db.file_path(file_id);
            msg.push_str(&format!("//- {file_path}\n"));
            msg.push_str(&debug_lower_result(salsa_db, lower_result));
        }

        msg
    }

    fn debug_lower_result(salsa_db: &dyn crate::Db, lower_result: &LowerResult) -> String {
        let mut msg = "".to_string();

        for item in &lower_result.top_level_items {
            msg.push_str(&debug_item(salsa_db, lower_result, item, 0));
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
        salsa_db: &dyn crate::Db,
        lower_result: &LowerResult,
        function_id: FunctionId,
        nesting: usize,
    ) -> String {
        let function = function_id.lookup(&lower_result.db);
        let block_ast_id = lower_result
            .item_tree
            .block_id_by_function(&function_id)
            .unwrap();
        let body_expr = lower_result
            .shared_ctx
            .function_body_by_block(block_ast_id)
            .unwrap();

        let name = if let Some(name) = function.name {
            name.text(salsa_db)
        } else {
            "<missing>"
        };
        let params = function
            .params
            .iter()
            .map(|param| {
                let param = param.lookup(&lower_result.db);
                let name = if let Some(name) = param.name {
                    name.text(salsa_db)
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

        let body = debug_expr(salsa_db, lower_result, body_expr, nesting);
        let is_entry_point = lower_result.entry_point == Some(function_id);
        format!(
            "{}fn {}{name}({params}) -> {return_type} {body}\n",
            indent(nesting),
            if is_entry_point { "entry:" } else { "" }
        )
    }

    fn debug_module(
        salsa_db: &dyn crate::Db,
        lower_result: &LowerResult,
        module_id: ModuleId,
        nesting: usize,
    ) -> String {
        let curr_indent = indent(nesting);

        let module = module_id.lookup(&lower_result.db);
        let module_name = module.name.text(salsa_db);

        match &module.kind {
            ModuleKind::Inline { items } => {
                let mut module_str = "".to_string();
                module_str.push_str(&format!("{curr_indent}mod {module_name} {{\n"));
                for (i, item) in items.iter().enumerate() {
                    module_str.push_str(&debug_item(salsa_db, lower_result, item, nesting + 1));
                    if i == items.len() - 1 {
                        continue;
                    }

                    module_str.push('\n');
                }
                module_str.push_str(&format!("{curr_indent}}}\n"));
                module_str
            }
            ModuleKind::Outline => {
                format!("{curr_indent}mod {module_name};\n")
            }
        }
    }

    fn debug_use_item(
        salsa_db: &dyn crate::Db,
        lower_result: &LowerResult,
        use_item_id: UseItemId,
    ) -> String {
        let use_item = use_item_id.lookup(&lower_result.db);
        let path_name = debug_path(salsa_db, &use_item.path);
        let item_name = use_item.name.text(salsa_db);

        format!("{path_name}::{item_name}")
    }

    fn debug_item(
        salsa_db: &dyn crate::Db,
        lower_result: &LowerResult,
        item: &ItemDefId,
        nesting: usize,
    ) -> String {
        match item {
            ItemDefId::Function(function) => {
                debug_function(salsa_db, lower_result, *function, nesting)
            }
            ItemDefId::Module(module) => debug_module(salsa_db, lower_result, *module, nesting),
            ItemDefId::UseItem(use_item) => debug_use_item(salsa_db, lower_result, *use_item),
        }
    }

    fn debug_stmt(
        salsa_db: &dyn crate::Db,
        lower_result: &LowerResult,
        stmt: &Stmt,
        nesting: usize,
    ) -> String {
        match stmt {
            Stmt::VariableDef { name, value } => {
                let name = name.text(salsa_db);
                let expr_str = debug_expr(
                    salsa_db,
                    lower_result,
                    value.lookup(&lower_result.shared_ctx),
                    nesting,
                );
                format!("{}let {name} = {expr_str}\n", indent(nesting))
            }
            Stmt::ExprStmt {
                expr,
                has_semicolon,
            } => format!(
                "{}{}{}\n",
                indent(nesting),
                debug_expr(
                    salsa_db,
                    lower_result,
                    expr.lookup(&lower_result.shared_ctx),
                    nesting
                ),
                if *has_semicolon { ";" } else { "" }
            ),
            Stmt::Item { item } => debug_item(salsa_db, lower_result, item, nesting),
        }
    }

    fn debug_expr(
        salsa_db: &dyn crate::Db,
        lower_result: &LowerResult,
        expr: &Expr,
        nesting: usize,
    ) -> String {
        match expr {
            Expr::Symbol(symbol) => debug_symbol(salsa_db, lower_result, symbol, nesting),
            Expr::Literal(literal) => match literal {
                Literal::Bool(b) => b.to_string(),
                Literal::Char(c) => format!("'{c}'"),
                Literal::String(s) => format!("\"{s}\""),
                Literal::Integer(i) => i.to_string(),
            },
            Expr::Binary { op, lhs, rhs } => {
                let op = match op {
                    ast::BinaryOp::Add(_) => "+",
                    ast::BinaryOp::Sub(_) => "-",
                    ast::BinaryOp::Mul(_) => "*",
                    ast::BinaryOp::Div(_) => "/",
                    ast::BinaryOp::Equal(_) => "==",
                    ast::BinaryOp::GreaterThan(_) => ">",
                    ast::BinaryOp::LessThan(_) => "<",
                };
                let lhs_str = debug_expr(
                    salsa_db,
                    lower_result,
                    lhs.lookup(&lower_result.shared_ctx),
                    nesting,
                );
                let rhs_str = debug_expr(
                    salsa_db,
                    lower_result,
                    rhs.lookup(&lower_result.shared_ctx),
                    nesting,
                );
                format!("{lhs_str} {op} {rhs_str}")
            }
            Expr::Unary { op, expr } => {
                let op = match op {
                    ast::UnaryOp::Neg(_) => "-",
                    ast::UnaryOp::Not(_) => "!",
                };
                let expr_str = debug_expr(
                    salsa_db,
                    lower_result,
                    expr.lookup(&lower_result.shared_ctx),
                    nesting,
                );
                format!("{op}{expr_str}")
            }
            Expr::Call { callee, args } => {
                let callee = debug_symbol(salsa_db, lower_result, callee, nesting);
                let args = args
                    .iter()
                    .map(|arg| {
                        debug_expr(
                            salsa_db,
                            lower_result,
                            arg.lookup(&lower_result.shared_ctx),
                            nesting,
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{callee}({args})")
            }
            Expr::Block(block) => {
                let mut msg = "{\n".to_string();
                for stmt in &block.stmts {
                    msg.push_str(&debug_stmt(salsa_db, lower_result, stmt, nesting + 1));
                }
                if let Some(tail) = block.tail {
                    msg.push_str(&format!(
                        "{}expr:{}\n",
                        indent(nesting + 1),
                        debug_expr(
                            salsa_db,
                            lower_result,
                            tail.lookup(&lower_result.shared_ctx),
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
                    salsa_db,
                    lower_result,
                    condition.lookup(&lower_result.shared_ctx),
                    nesting,
                ));
                msg.push(' ');
                msg.push_str(&debug_expr(
                    salsa_db,
                    lower_result,
                    then_branch.lookup(&lower_result.shared_ctx),
                    nesting,
                ));

                if let Some(else_branch) = else_branch {
                    msg.push_str(" else ");
                    msg.push_str(&debug_expr(
                        salsa_db,
                        lower_result,
                        else_branch.lookup(&lower_result.shared_ctx),
                        nesting,
                    ));
                }

                msg
            }
            Expr::Return { value } => {
                let mut msg = "return".to_string();
                if let Some(value) = value {
                    msg.push_str(&format!(
                        " {}",
                        &debug_expr(
                            salsa_db,
                            lower_result,
                            value.lookup(&lower_result.shared_ctx),
                            nesting,
                        )
                    ));
                }

                msg
            }
            Expr::Missing => "<missing>".to_string(),
        }
    }

    fn debug_symbol(
        salsa_db: &dyn crate::Db,
        lower_result: &LowerResult,
        symbol: &Symbol,
        nesting: usize,
    ) -> String {
        match symbol {
            Symbol::Local { name, expr } => match expr.lookup(&lower_result.shared_ctx) {
                Expr::Symbol { .. }
                | Expr::Binary { .. }
                | Expr::Missing
                | Expr::Literal(_)
                | Expr::Unary { .. }
                | Expr::Call { .. }
                | Expr::If { .. }
                | Expr::Return { .. } => debug_expr(
                    salsa_db,
                    lower_result,
                    expr.lookup(&lower_result.shared_ctx),
                    nesting,
                ),
                Expr::Block { .. } => name.text(salsa_db).to_string(),
            },
            Symbol::Param { name, .. } => {
                let name = name.text(salsa_db);
                format!("param:{name}")
            }
            Symbol::Function { path, .. } => {
                let name = debug_path(salsa_db, path);
                format!("fn:{name}")
            }
            Symbol::Missing { .. } => "<missing>".to_string(),
        }
    }

    fn debug_path(salsa_db: &dyn crate::Db, path: &Path) -> String {
        path.segments
            .iter()
            .map(|segment| segment.text(salsa_db).to_string())
            .collect::<Vec<_>>()
            .join("::")
    }

    fn parse(input: &str) -> ast::SourceFile {
        ast::SourceFile::cast(parser::parse(input).syntax()).unwrap()
    }

    fn check_in_root_file(fixture: &str, expected: Expect) {
        let mut fixture = fixture.to_string();
        fixture.insert_str(0, "//- /main.nail\n");

        let salsa_db = SalsaDatabase::default();
        let source_db = FixtureDatabase::new(&fixture);
        let source_root_file_id = source_db.source_root();
        let source_root_file_content = source_db.content(source_root_file_id).unwrap();

        let source_file = parse(source_root_file_content);
        let result = lower_root(&salsa_db, source_root_file_id, source_file);

        expected.assert_eq(&debug_lower_result(&salsa_db, &result));
    }

    /// ルートファイルからパースして、すべてのモジュールの期待結果をテストする
    fn check_modules_start_with_root_file(fixture: &str, expected: Expect) {
        let salsa_db = SalsaDatabase::default();
        let mut source_db = FixtureDatabase::new(fixture);

        let modules = parse_pod(&salsa_db, "/main.nail", &mut source_db);

        expected.assert_eq(&debug_modules(&salsa_db, &modules, &source_db));
    }

    #[test]
    fn fibonacci() {
        check_in_root_file(
            r#"
                fn fibonacci(x: int) -> int {
                    if x == 0 {
                        0
                    } else {
                        if x == 1 {
                            1
                        } else {
                            fibonacci(x - 1) + fibonacci(x - 2)
                        }
                    }
                }
                fn main() -> int {
                    fibonacci(15)
                }
            "#,
            expect![[r#"
                fn fibonacci(x: int) -> int {
                    expr:if param:x == 0 {
                        expr:0
                    } else {
                        expr:if param:x == 1 {
                            expr:1
                        } else {
                            expr:fn:fibonacci(param:x - 1) + fn:fibonacci(param:x - 2)
                        }
                    }
                }
                fn entry:main() -> int {
                    expr:fn:fibonacci(15)
                }
            "#]],
        );
    }

    #[test]
    fn entry_point() {
        check_in_root_file(
            r#"
                fn no_main() { }
            "#,
            expect![[r#"
                fn no_main() -> () {
                }
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
    fn lower_last_expr_stmt_with_semicolon_only_as_expr() {
        check_in_root_file(
            r#"
                fn main() {
                    123
                }
                fn main_2() {
                    123;
                }
                fn main_3() {
                    123
                    456;
                }
                fn main_4() {
                    123;
                    456;
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:123
                }
                fn main_2() -> () {
                    123;
                }
                fn main_3() -> () {
                    123
                    456;
                }
                fn main_4() -> () {
                    123;
                    456;
                }
            "#]],
        );
    }

    #[test]
    fn lower_binary_expr() {
        check_in_root_file(
            r#"
                fn main() {
                    1 + 2;
                    1 - 2;
                    1 * 2;
                    1 / 2;
                    1 == 2;
                    1 < 2;
                    1 > 2;
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    1 + 2;
                    1 - 2;
                    1 * 2;
                    1 / 2;
                    1 == 2;
                    1 < 2;
                    1 > 2;
                }
            "#]],
        );
    }

    #[test]
    fn lower_binary_expr_without_rhs() {
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
            r#"
                fn main() {
                    -10;
                    !20;
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    -10;
                    !20;
                }
            "#]],
        );
    }

    #[test]
    fn lower_unary_expr_without_expr() {
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
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
        check_in_root_file(
            "fn a(a, )",
            expect![[r#"
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
    }

    #[test]
    fn function_missing_name() {
        check_in_root_file(
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
        check_in_root_file(
            "fn main() { a() }",
            expect![[r#"
                fn entry:main() -> () {
                    expr:<missing>()
                }
            "#]],
        );

        check_in_root_file(
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
        check_in_root_file(
            "fn main() { a(10, 20) }",
            expect![[r#"
                fn entry:main() -> () {
                    expr:<missing>(10, 20)
                }
            "#]],
        );

        check_in_root_file(
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

        check_in_root_file(
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
                    fn:aaa("aaa", true);
                }
            "#]],
        );
    }

    #[test]
    fn if_expr() {
        check_in_root_file(
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
                    expr:if true {
                        expr:10
                    } else {
                        expr:20
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() {
                    if true {
                        10
                    }
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:if true {
                        expr:10
                    }
                }
            "#]],
        );
    }

    #[test]
    fn if_expr_block_condition() {
        check_in_root_file(
            r#"
                fn main() {
                    if { true } {
                        10
                    } else {
                        20
                    }
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:if {
                        expr:true
                    } {
                        expr:10
                    } else {
                        expr:20
                    }
                }
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() {
                    if { true } {
                        10
                    }
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:if {
                        expr:true
                    } {
                        expr:10
                    }
                }
            "#]],
        );
    }

    #[test]
    fn return_expr() {
        check_in_root_file(
            r#"
                fn main() {
                    return
                }
                fn test1() {
                    return 10
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:return
                }
                fn test1() -> () {
                    expr:return 10
                }
            "#]],
        );
    }

    #[test]
    fn modules() {
        check_in_root_file(
            r#"
                fn main() {
                    return
                }
                mod module_aaa {
                    mod module_bbb {
                        fn function_aaa() -> int {
                            mod module_ccc {
                                fn function_bbb() -> int {
                                    10
                                }
                            }

                            20
                        }
                    }

                    fn function_ccc() -> int {
                        30
                    }
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    expr:return
                }
                mod module_aaa {
                    mod module_bbb {
                        fn function_aaa() -> int {
                            mod module_ccc {
                                fn function_bbb() -> int {
                                    expr:10
                                }
                            }
                            expr:20
                        }
                    }

                    fn function_ccc() -> int {
                        expr:30
                    }
                }
            "#]],
        );
    }

    #[test]
    fn can_ref_same_scope() {
        check_in_root_file(
            r#"
                fn main() {
                    mod_aaa::fn_aaa();
                    bbb();

                    mod mod_aaa {
                        fn fn_aaa() {
                        }
                    }

                    fn bbb() {
                    }
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    fn:mod_aaa::fn_aaa();
                    fn:bbb();
                    mod mod_aaa {
                        fn fn_aaa() -> () {
                        }
                    }
                    fn bbb() -> () {
                    }
                }
            "#]],
        );
    }

    #[test]
    fn can_ref_in_parent_scope() {
        check_in_root_file(
            r#"
                fn main() {
                    mod_aaa::fn_aaa();
                }
                mod mod_aaa {
                    fn fn_aaa() {
                        mod_bbb::fn_bbb();
                        mod_aaa::mod_bbb::fn_bbb();
                    }
                    mod mod_bbb {
                        fn fn_bbb() {
                        }
                    }
                }
            "#,
            expect![[r#"
                fn entry:main() -> () {
                    fn:mod_aaa::fn_aaa();
                }
                mod mod_aaa {
                    fn fn_aaa() -> () {
                        fn:mod_bbb::fn_bbb();
                        fn:mod_aaa::mod_bbb::fn_bbb();
                    }

                    mod mod_bbb {
                        fn fn_bbb() -> () {
                        }
                    }
                }
            "#]],
        );
    }

    #[test]
    #[should_panic]
    fn inline_module() {
        check_modules_start_with_root_file(
            r#"
                //- /main.nail
                mod mod_aaa;

                fn main() {
                    mod_aaa::fn_aaa();
                }

                //- /mod_aaa.nail
                fn fn_aaa() {
                }
            "#,
            expect![[r#"
                //- /main.nail
                mod mod_aaa;
                fn entry:main() -> () {
                    mod_aaa::fn_aaa();
                }

                //- /mod_aaa.nail
                fn fn_aaa() -> () {
                }
            "#]],
        );
    }
}
