mod scopes;

use std::collections::HashMap;

use ast::AstNode;
use la_arena::{Arena, Idx};

use crate::{
    body::scopes::ExprScopes, item::ParamData, AstPtr, Block, Expr, ExprSource, Function,
    FunctionSource, HirFileSourceMap, HirMasterDatabase, InFile, Item, Literal, Module, ModuleKind,
    NailFile, Name, NameSolutionPath, Param, Path, Stmt, Symbol, Type, UseItem,
};

/// 式を一意に識別するためのID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(Idx<Expr>);
impl ExprId {
    /// このIDに対応する式を取得する
    pub fn lookup(self, ctx: &HirFileDatabase) -> &Expr {
        &ctx.exprs[self.0]
    }
}
impl ExprId {
    pub fn text_range(
        &self,
        db: &dyn HirMasterDatabase,
        source_map: &HirFileSourceMap,
    ) -> ast::TextRange {
        source_map
            .source_by_expr(db)
            .get(self)
            .expect("BUG: There should always be an expression in the source map.")
            .clone()
            .value
            .node
            .text_range()
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
pub struct FunctionBodyId(ExprId);

/// 1ファイルに対応するDB
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct HirFileDatabase {
    functions: Vec<Function>,
    modules: Vec<Module>,

    params: Arena<ParamData>,

    exprs: Arena<Expr>,
    function_body_by_ast_block: HashMap<ast::BlockExpr, FunctionBodyId>,
}
impl HirFileDatabase {
    /// 空のDBを作成する
    pub fn new() -> Self {
        Self {
            functions: vec![],
            modules: vec![],
            params: Arena::new(),
            exprs: Arena::new(),
            function_body_by_ast_block: HashMap::new(),
        }
    }

    /// 1ファイル中のHIR化で現れた関数一覧を返します。
    pub fn functions(&self) -> &[Function] {
        &self.functions
    }

    /// 1ファイル中のHIR化で現れたモジュール一覧を返します。
    pub fn modules(&self) -> &[Module] {
        &self.modules
    }

    /// ASTブロックIDを元に関数本体IDを取得する
    pub fn function_body_id_by_ast_block(
        &self,
        ast_block: ast::BlockExpr,
    ) -> Option<FunctionBodyId> {
        self.function_body_by_ast_block.get(&ast_block).copied()
    }

    /// パラメータIDを元にパラメータを取得する
    pub fn param_data(&self, param: Param) -> &ParamData {
        &self.params[param.0]
    }

    /// ASTブロックIDを元に関数本体を取得する
    ///
    /// 現状、関数本体は[Expr::Block]として表現されているため、[Expr]を返す
    pub fn function_body_by_ast_block(&self, ast_block: ast::BlockExpr) -> Option<&Expr> {
        self.function_body_id_by_ast_block(ast_block)
            .map(|body_id| &self.exprs[body_id.0 .0])
    }

    /// 式を保存し、参照するためのIDを返す
    pub(crate) fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        ExprId(self.exprs.alloc(expr))
    }

    /// パラメータを保存し、参照するためのIDを返す
    pub(crate) fn alloc_param(&mut self, param: ParamData) -> Param {
        Param(self.params.alloc(param))
    }
}

/// ASTを元にトップレベルあるいは関数のHIRを構築するためのコンテキスト/ビルダー
///
/// ユースケースはトップレベルと関数本体の2種類あります。
/// トップレベルの場合、`params`は空で指定します。
/// 関数本体の場合、`params`に関数パラメータを指定します。
/// `params`は、名前解決に使用されます。
#[derive(Debug)]
pub struct BodyLower<'a> {
    file: NailFile,
    scopes: ExprScopes,
    params: HashMap<Name, Param>,
    hir_file_db: &'a mut HirFileDatabase,
    pub source_by_expr: HashMap<ExprId, ExprSource>,
    pub source_by_function: HashMap<Function, FunctionSource>,
}

impl<'a> BodyLower<'a> {
    /// 空のコンテキストを作成する
    pub(super) fn new(
        file: NailFile,
        params: HashMap<Name, Param>,
        hir_file_db: &'a mut HirFileDatabase,
    ) -> Self {
        Self {
            file,
            scopes: ExprScopes::new(),
            params,
            hir_file_db,
            source_by_expr: HashMap::new(),
            source_by_function: HashMap::new(),
        }
    }

    /// トップレベルに定義された[ast::Item]を元に、トップレベルのHIRを構築する
    pub(super) fn lower_toplevel(
        &mut self,
        db: &dyn HirMasterDatabase,
        ast: ast::Item,
    ) -> Option<Item> {
        match ast {
            ast::Item::FunctionDef(def) => Some(Item::Function(self.lower_function(db, def)?)),
            ast::Item::Module(module) => Some(Item::Module(self.lower_module(db, module)?)),
            ast::Item::Use(r#use) => Some(Item::UseItem(self.lower_use(db, r#use)?)),
        }
    }

    /// 関数のHIRを構築します。
    fn lower_function(
        &mut self,
        db: &dyn HirMasterDatabase,
        def: ast::FunctionDef,
    ) -> Option<Function> {
        let name = def
            .name()
            .map(|name| Name::new(db, name.name().to_string()))?;

        let ast_block = def.body()?;
        let params = def
            .params()?
            .params()
            .enumerate()
            .map(|(pos, param)| {
                let name = param
                    .name()
                    .map(|name| Name::new(db, name.name().to_string()));
                let ty = self.lower_ty(param.ty());
                Param::new(self.hir_file_db, name, ty, pos)
            })
            .collect::<Vec<_>>();
        let param_by_name = params
            .iter()
            .filter_map(|param| param.data(self.hir_file_db).name.map(|name| (name, *param)))
            .collect::<HashMap<_, _>>();
        let return_type = if let Some(return_type) = def.return_type() {
            self.lower_ty(return_type.ty())
        } else {
            Type::Unit
        };

        let function = Function::new(db, name, params, param_by_name, return_type, def.clone());
        self.hir_file_db.functions.push(function);
        self.source_by_function.insert(
            function,
            FunctionSource {
                file: self.file,
                value: def,
            },
        );

        let mut body_lower = BodyLower::new(
            self.file,
            function.param_by_name(db).clone(),
            self.hir_file_db,
        );
        let body_expr = body_lower.lower_block(db, &ast_block);
        let source_by_expr = body_lower.source_by_expr;
        let source_by_function = body_lower.source_by_function;

        let body_expr = self.alloc_expr(
            &ast::Expr::cast(ast_block.syntax().clone()).unwrap(),
            body_expr,
        );

        // キーはファイル内で一意なので重複する心配はない
        self.source_by_expr.extend(source_by_expr.into_iter());
        self.source_by_function
            .extend(source_by_function.into_iter());
        self.hir_file_db
            .function_body_by_ast_block
            .insert(ast_block, FunctionBodyId(body_expr));

        Some(function)
    }

    /// モジュールのHIRを構築します。
    ///
    /// アウトラインモジュール(`mod aaa;`)のパースは[crate::parse_modules]で行います。
    /// この中では、トラバースするために各アイテムのHIR化を行っています。
    /// また、[ItemDefId]を返すために、アウトラインモジュールも返します。
    fn lower_module(
        &mut self,
        db: &dyn HirMasterDatabase,
        ast_module: ast::Module,
    ) -> Option<Module> {
        if let Some(name) = ast_module.name() {
            let module_name = Name::new(db, name.name().to_string());

            let module_kind = if let Some(item_list) = ast_module.items() {
                let mut items = vec![];
                for item in item_list.items() {
                    if let Some(item) = self.lower_item(db, item) {
                        items.push(item);
                    }
                }
                ModuleKind::Inline { items }
            } else {
                ModuleKind::Outline
            };
            let module = Module::new(db, module_name, module_kind);
            self.hir_file_db.modules.push(module);

            Some(module)
        } else {
            None
        }
    }

    fn lower_use(&mut self, db: &dyn HirMasterDatabase, ast_use: ast::Use) -> Option<UseItem> {
        let use_path = ast_use.path()?.segments().collect::<Vec<_>>();
        match use_path.as_slice() {
            [] => unreachable!("use path should not be empty"),
            [path @ .., name] => {
                let name = Name::new(db, name.name().unwrap().name().to_string());
                let segments = path
                    .iter()
                    .map(|segment| Name::new(db, segment.name().unwrap().name().to_string()))
                    .collect::<Vec<_>>();
                let path = Path::new(db, segments);
                let use_item = UseItem::new(db, name, path);

                Some(use_item)
            }
        }
    }

    /// 型を構築します。
    /// 型を構築できなかった場合は`Type::Unknown`を返します。
    fn lower_ty(&mut self, ty: Option<ast::Type>) -> Type {
        let ident = match ty.and_then(|ty| ty.ty()) {
            Some(ident) => ident,
            None => return Type::Unknown,
        };

        let type_name = ident.name();
        match type_name {
            "int" => Type::Integer,
            "string" => Type::String,
            "char" => Type::Char,
            "bool" => Type::Boolean,
            _ => Type::Unknown,
        }
    }

    fn lower_stmt(&mut self, db: &dyn HirMasterDatabase, ast_stmt: ast::Stmt) -> Option<Stmt> {
        let result = match ast_stmt {
            ast::Stmt::VariableDef(def) => {
                let expr = self.lower_expr(db, def.value());
                let name = Name::new(db, def.name()?.name().to_owned());
                self.scopes.define(name, expr);
                Stmt::VariableDef { name, value: expr }
            }
            ast::Stmt::ExprStmt(ast) => {
                let expr = self.lower_expr(db, ast.expr());
                Stmt::ExprStmt {
                    expr,
                    has_semicolon: ast.semicolon().is_some(),
                }
            }
            ast::Stmt::Item(item) => {
                let item = self.lower_item(db, item)?;
                Stmt::Item { item }
            }
        };

        Some(result)
    }

    fn lower_item(&mut self, db: &dyn HirMasterDatabase, ast_item: ast::Item) -> Option<Item> {
        match ast_item {
            ast::Item::FunctionDef(def) => Some(Item::Function(self.lower_function(db, def)?)),
            ast::Item::Module(module) => Some(Item::Module(self.lower_module(db, module)?)),
            ast::Item::Use(r#use) => Some(Item::UseItem(self.lower_use(db, r#use)?)),
        }
    }

    fn lower_expr(&mut self, db: &dyn HirMasterDatabase, ast_expr: Option<ast::Expr>) -> ExprId {
        if let Some(ast) = ast_expr {
            let expr = match &ast {
                ast::Expr::BinaryExpr(ast) => self.lower_binary(db, ast),
                ast::Expr::Literal(ast) => self.lower_literal(ast),
                ast::Expr::ParenExpr(ast) => return self.lower_expr(db, ast.expr()),
                ast::Expr::UnaryExpr(ast) => self.lower_unary(db, ast),
                ast::Expr::PathExpr(ast) => self.lower_path_expr(db, ast),
                ast::Expr::CallExpr(ast) => self.lower_call(db, ast),
                ast::Expr::BlockExpr(ast) => self.lower_block(db, ast),
                ast::Expr::IfExpr(ast) => self.lower_if(db, ast),
                ast::Expr::ReturnExpr(ast) => self.lower_return(db, ast),
                ast::Expr::LoopExpr(ast) => self.lower_loop(db, ast),
                ast::Expr::ContinueExpr(_ast) => Expr::Continue,
                ast::Expr::BreakExpr(ast) => self.lower_break(db, ast),
                ast::Expr::WhileExpr(ast) => self.lower_while(db, ast),
            };
            self.alloc_expr(&ast, expr)
        } else {
            self.hir_file_db.alloc_expr(Expr::Missing)
        }
    }

    fn lower_literal(&self, ast_literal: &ast::Literal) -> Expr {
        match ast_literal.kind() {
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

    fn alloc_expr(&mut self, ast_expr: &ast::Expr, expr: Expr) -> ExprId {
        let expr_id = self.hir_file_db.alloc_expr(expr);
        self.source_by_expr.insert(
            expr_id,
            InFile {
                file: self.file,
                value: AstPtr {
                    node: syntax::SyntaxNodePtr::new(ast_expr.syntax()),
                    _ty: std::marker::PhantomData,
                },
            },
        );

        expr_id
    }

    /// 脱糖された式を保存します。
    ///
    /// 例えば、`if`式は`if`式のまま保存されますが、`while`式は`loop`式と`if`式に脱糖されます。
    /// そのため、`while`式を保存する際には、`loop`式と`if`式を保存する必要があります。
    /// この関数は、`loop`式と`if`式を保存するために使用されます。
    /// また、`loop`式と`if`式は、`while`式のASTノードを参照するため、`while`式のASTノードを保存する必要があります。
    /// そのため、`while`式を保存する際には、`while`式のASTノードを保存する必要があります。
    /// この関数は、`while`式のASTノードを保存するために使用されます。
    fn alloc_expr_desugared(&mut self, from_ast_expr: &ast::Expr, expr: Expr) -> ExprId {
        let expr_id = self.hir_file_db.alloc_expr(expr);
        self.source_by_expr.insert(
            expr_id,
            InFile {
                file: self.file,
                value: AstPtr {
                    node: syntax::SyntaxNodePtr::new(from_ast_expr.syntax()),
                    _ty: std::marker::PhantomData,
                },
            },
        );

        expr_id
    }

    fn lower_binary(
        &mut self,
        db: &dyn HirMasterDatabase,
        ast_binary_expr: &ast::BinaryExpr,
    ) -> Expr {
        let op = ast_binary_expr.op().unwrap();

        let lhs = self.lower_expr(db, ast_binary_expr.lhs());
        let rhs = self.lower_expr(db, ast_binary_expr.rhs());

        Expr::Binary { op, lhs, rhs }
    }

    fn lower_unary(&mut self, db: &dyn HirMasterDatabase, ast_unary_expr: &ast::UnaryExpr) -> Expr {
        let op = ast_unary_expr.op().unwrap();

        let expr = self.lower_expr(db, ast_unary_expr.expr());

        Expr::Unary { op, expr }
    }

    fn lower_path_expr(&mut self, db: &dyn HirMasterDatabase, ast_path: &ast::PathExpr) -> Expr {
        let path = Path::new(
            db,
            ast_path
                .path()
                .unwrap()
                .segments()
                .map(|segment| Name::new(db, segment.name().unwrap().name().to_string()))
                .collect(),
        );
        let symbol = self.lookup_path(db, path);
        Expr::Symbol(symbol)
    }

    fn lookup_path(&mut self, db: &dyn HirMasterDatabase, path: Path) -> Symbol {
        match path.segments(db).as_slice() {
            [name] => {
                let name = *name;
                if let Some(expr) = self.scopes.lookup_in_only_current_scope(name) {
                    Symbol::Local { name, expr }
                } else if let Some(param_id) = self.lookup_param(name) {
                    Symbol::Param {
                        name,
                        param: param_id,
                    }
                } else if let Some(expr) = self.scopes.lookup(name) {
                    Symbol::Local { name, expr }
                } else {
                    Symbol::Missing {
                        path: NameSolutionPath::new(db, Path::new(db, vec![name])),
                    }
                }
            }
            [_segments @ .., _item_segment] => Symbol::Missing {
                path: NameSolutionPath::new(db, path),
            },
            [] => unreachable!(),
        }
    }

    fn lookup_param(&self, name: Name) -> Option<Param> {
        self.params.get(&name).copied()
    }

    fn lower_call(&mut self, db: &dyn HirMasterDatabase, ast_call: &ast::CallExpr) -> Expr {
        let args = ast_call.args().unwrap();
        let args = args
            .args()
            .map(|arg| self.lower_expr(db, arg.expr()))
            .collect();
        let callee = match self
            .lower_expr(db, ast_call.callee())
            .lookup(self.hir_file_db)
        {
            Expr::Symbol(symbol) => symbol,
            Expr::Binary { .. } => todo!(),
            Expr::Literal(_) => todo!(),
            Expr::Unary { .. } => todo!(),
            Expr::Call { .. } => todo!(),
            Expr::Block(_) => todo!(),
            Expr::If { .. } => todo!(),
            Expr::Return { .. } => todo!(),
            Expr::Loop { .. } => todo!(),
            Expr::Continue => todo!(),
            Expr::Break { .. } => todo!(),
            Expr::Missing => todo!(),
        };

        Expr::Call {
            callee: callee.clone(),
            args,
        }
    }

    fn lower_block(&mut self, db: &dyn HirMasterDatabase, ast_block: &ast::BlockExpr) -> Expr {
        self.scopes.enter(ast_block.clone());

        let mut stmts = vec![];
        for stmt in ast_block.stmts() {
            if let Some(stmt) = self.lower_stmt(db, stmt) {
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

        Expr::Block(Block { stmts, tail })
    }

    fn lower_if(&mut self, db: &dyn HirMasterDatabase, ast_if: &ast::IfExpr) -> Expr {
        let condition = self.lower_expr(db, ast_if.condition());

        let ast_then_branch = ast_if.then_branch().unwrap();
        let then_branch = self.lower_block(db, &ast_then_branch);
        let then_branch = self.alloc_expr(
            &ast::Expr::cast(ast_then_branch.syntax().clone()).unwrap(),
            then_branch,
        );

        let else_branch = if let Some(ast_else_branch) = ast_if.else_branch() {
            let else_branch = self.lower_block(db, &ast_else_branch);
            Some(self.alloc_expr(
                &ast::Expr::cast(ast_else_branch.syntax().clone()).unwrap(),
                else_branch,
            ))
        } else {
            None
        };

        Expr::If {
            condition,
            then_branch,
            else_branch,
        }
    }

    fn lower_return(&mut self, db: &dyn HirMasterDatabase, ast_return: &ast::ReturnExpr) -> Expr {
        let value = if let Some(value) = ast_return.value() {
            let value = self.lower_expr(db, Some(value));
            Some(value)
        } else {
            None
        };

        Expr::Return { value }
    }

    fn lower_loop(&mut self, db: &dyn HirMasterDatabase, ast_loop: &ast::LoopExpr) -> Expr {
        if let Some(body) = ast_loop.body() {
            let block = self.lower_block(db, &body);
            Expr::Loop {
                block: self.alloc_expr(&ast::Expr::cast(ast_loop.syntax().clone()).unwrap(), block),
            }
        } else {
            Expr::Missing
        }
    }

    fn lower_break(&mut self, db: &dyn HirMasterDatabase, ast_loop: &ast::BreakExpr) -> Expr {
        if let Some(value) = ast_loop.value() {
            let value = self.lower_expr(db, Some(value));
            Expr::Break { value: Some(value) }
        } else {
            Expr::Break { value: None }
        }
    }

    /// while式を構築します。
    ///
    /// while式はloop式とIf式に脱糖されます。
    /// ```nail
    /// let a = 0;
    /// while a > 10 {
    ///     a = a + 1
    /// }
    /// ```
    /// ↓
    /// ```nail
    /// let a = 0;
    /// loop {
    ///     if (a > 10) {
    ///         a = a + 1
    ///     } else {
    ///         break;
    ///     }
    /// }
    /// ```
    ///
    fn lower_while(&mut self, db: &dyn HirMasterDatabase, ast_while: &ast::WhileExpr) -> Expr {
        let condition = self.lower_expr(db, ast_while.condition());

        if let Some(ast_body) = ast_while.body() {
            // while式の条件が一致する間実行するブロック
            let do_block = self.lower_expr(
                db,
                Some(ast::Expr::cast(ast_body.syntax().clone()).unwrap()),
            );

            // while式の条件が一致しない場合にbreakするためのブロック
            let break_block = {
                let break_block = Expr::Block(Block {
                    stmts: vec![Stmt::ExprStmt {
                        expr: self.alloc_expr_desugared(
                            &ast::Expr::cast(ast_while.syntax().clone()).unwrap(),
                            Expr::Break { value: None },
                        ),
                        has_semicolon: true,
                    }],
                    tail: None,
                });
                self.alloc_expr_desugared(&ast::Expr::WhileExpr(ast_while.clone()), break_block)
            };

            // while式を実行ブロックとbreakブロックとloop式とif式に脱糖する
            let loop_block = {
                let if_expr = Expr::If {
                    condition,
                    then_branch: do_block,
                    else_branch: Some(break_block),
                };

                Expr::Block(Block {
                    stmts: vec![Stmt::ExprStmt {
                        expr: self.alloc_expr_desugared(
                            &ast::Expr::cast(ast_while.syntax().clone()).unwrap(),
                            if_expr,
                        ),
                        has_semicolon: true,
                    }],
                    tail: None,
                })
            };
            let loop_block =
                self.alloc_expr_desugared(&ast::Expr::WhileExpr(ast_while.clone()), loop_block);
            Expr::Loop { block: loop_block }
        } else {
            Expr::Missing
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use super::*;
    use crate::{
        input::FixtureDatabase,
        name_resolver::{ModuleScopeOrigin, ResolutionMap, ResolutionStatus},
        parse_pods,
        testing::TestingDatabase,
        Function, HirFile, LowerError, Module, ModuleKind, Pod, Pods, UseItem,
    };

    fn indent(nesting: usize) -> String {
        "    ".repeat(nesting)
    }

    fn debug_pods(db: &dyn HirMasterDatabase, pods: &Pods) -> String {
        debug_pod(db, &pods.root_pod, &pods.resolution_map)
    }

    fn debug_pod(db: &dyn HirMasterDatabase, pod: &Pod, resolution_map: &ResolutionMap) -> String {
        let mut msg = "".to_string();

        msg.push_str("//- /main.nail\n");
        msg.push_str(&debug_hir_file(db, &pod.root_hir_file, resolution_map));

        for (file, hir_file) in pod.get_hir_files_order_registration_asc() {
            let file_path = file.file_path(db);
            msg.push_str(&format!("//- {}\n", file_path.to_string_lossy()));
            msg.push_str(&debug_hir_file(db, hir_file, resolution_map));
        }

        msg
    }

    fn debug_hir_file(
        db: &dyn HirMasterDatabase,
        hir_file: &HirFile,
        resolution_map: &ResolutionMap,
    ) -> String {
        let mut msg = "".to_string();

        for item in hir_file.top_level_items(db) {
            msg.push_str(&debug_item(db, hir_file, resolution_map, item, 0));
        }

        for error in hir_file.errors(db) {
            match error {
                LowerError::UndefinedEntryPoint => {
                    msg.push_str("error: Undefined entry point.(help: fn main() { ... })\n");
                }
            }
        }

        msg
    }

    fn debug_function(
        db: &dyn HirMasterDatabase,
        hir_file: &HirFile,
        rsolution_map: &ResolutionMap,
        function: Function,
        nesting: usize,
    ) -> String {
        let body_expr = hir_file
            .db(db)
            .function_body_by_ast_block(function.ast(db).body().unwrap())
            .unwrap();

        let name = function.name(db).text(db);
        let params = function
            .params(db)
            .iter()
            .map(|param| {
                let name = if let Some(name) = param.data(hir_file.db(db)).name {
                    name.text(db)
                } else {
                    "<missing>"
                };
                let ty = match param.data(hir_file.db(db)).ty {
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
        let return_type = match &function.return_type(db) {
            Type::Integer => "int",
            Type::String => "string",
            Type::Char => "char",
            Type::Boolean => "bool",
            Type::Unit => "()",
            Type::Unknown => "<unknown>",
        };

        let scope_origin = ModuleScopeOrigin::Function { origin: function };

        let Expr::Block(block) = body_expr else { panic!("Should be Block") };

        let mut body = "{\n".to_string();
        for stmt in &block.stmts {
            body.push_str(&debug_stmt(
                db,
                hir_file,
                rsolution_map,
                scope_origin,
                stmt,
                nesting + 1,
            ));
        }
        if let Some(tail) = block.tail {
            body.push_str(&format!(
                "{}expr:{}\n",
                indent(nesting + 1),
                debug_expr(db, hir_file, rsolution_map, scope_origin, tail, nesting + 1)
            ));
        }
        body.push_str(&format!("{}}}", indent(nesting)));

        let is_entry_point = hir_file.entry_point(db) == Some(function);
        format!(
            "{}fn {}{name}({params}) -> {return_type} {body}\n",
            indent(nesting),
            if is_entry_point { "entry:" } else { "" }
        )
    }

    fn debug_module(
        db: &dyn HirMasterDatabase,
        hir_file: &HirFile,
        resolution_map: &ResolutionMap,
        module: Module,
        nesting: usize,
    ) -> String {
        let _scope_origin = ModuleScopeOrigin::Module { origin: module };

        let curr_indent = indent(nesting);

        let module_name = module.name(db).text(db);

        match module.kind(db) {
            ModuleKind::Inline { items } => {
                let mut module_str = "".to_string();
                module_str.push_str(&format!("{curr_indent}mod {module_name} {{\n"));
                for (i, item) in items.iter().enumerate() {
                    module_str.push_str(&debug_item(
                        db,
                        hir_file,
                        resolution_map,
                        item,
                        nesting + 1,
                    ));
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

    fn debug_use_item(db: &dyn HirMasterDatabase, use_item: UseItem) -> String {
        let path_name = debug_path(db, use_item.path(db));
        let item_name = use_item.name(db).text(db);

        format!("{path_name}::{item_name}")
    }

    fn debug_item(
        db: &dyn HirMasterDatabase,
        hir_file: &HirFile,
        resolution_map: &ResolutionMap,
        item: &Item,
        nesting: usize,
    ) -> String {
        match item {
            Item::Function(function) => {
                debug_function(db, hir_file, resolution_map, *function, nesting)
            }
            Item::Module(module) => debug_module(db, hir_file, resolution_map, *module, nesting),
            Item::UseItem(use_item) => debug_use_item(db, *use_item),
        }
    }

    fn debug_stmt(
        db: &dyn HirMasterDatabase,
        hir_file: &HirFile,
        resolution_map: &ResolutionMap,
        scope_origin: ModuleScopeOrigin,
        stmt: &Stmt,
        nesting: usize,
    ) -> String {
        match stmt {
            Stmt::VariableDef { name, value } => {
                let name = name.text(db);
                let expr_str =
                    debug_expr(db, hir_file, resolution_map, scope_origin, *value, nesting);
                format!("{}let {name} = {expr_str}\n", indent(nesting))
            }
            Stmt::ExprStmt {
                expr,
                has_semicolon,
            } => format!(
                "{}{}{}\n",
                indent(nesting),
                debug_expr(db, hir_file, resolution_map, scope_origin, *expr, nesting),
                if *has_semicolon { ";" } else { "" }
            ),
            Stmt::Item { item } => debug_item(db, hir_file, resolution_map, item, nesting),
        }
    }

    fn debug_expr(
        db: &dyn HirMasterDatabase,
        hir_file: &HirFile,
        resolution_map: &ResolutionMap,
        scope_origin: ModuleScopeOrigin,
        expr_id: ExprId,
        nesting: usize,
    ) -> String {
        match expr_id.lookup(hir_file.db(db)) {
            Expr::Symbol(symbol) => {
                debug_symbol(db, hir_file, resolution_map, scope_origin, symbol, nesting)
            }
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
                    ast::BinaryOp::Assign(_) => "=",
                };
                let lhs_str = debug_expr(db, hir_file, resolution_map, scope_origin, *lhs, nesting);
                let rhs_str = debug_expr(db, hir_file, resolution_map, scope_origin, *rhs, nesting);
                format!("{lhs_str} {op} {rhs_str}")
            }
            Expr::Unary { op, expr } => {
                let op = match op {
                    ast::UnaryOp::Neg(_) => "-",
                    ast::UnaryOp::Not(_) => "!",
                };
                let expr_str =
                    debug_expr(db, hir_file, resolution_map, scope_origin, *expr, nesting);
                format!("{op}{expr_str}")
            }
            Expr::Call { callee, args } => {
                let callee =
                    debug_symbol(db, hir_file, resolution_map, scope_origin, callee, nesting);
                let args = args
                    .iter()
                    .map(|arg| {
                        debug_expr(db, hir_file, resolution_map, scope_origin, *arg, nesting)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{callee}({args})")
            }
            Expr::Block(block) => {
                let scope_origin = ModuleScopeOrigin::Block { origin: expr_id };

                let mut msg = "{\n".to_string();
                for stmt in &block.stmts {
                    msg.push_str(&debug_stmt(
                        db,
                        hir_file,
                        resolution_map,
                        scope_origin,
                        stmt,
                        nesting + 1,
                    ));
                }
                if let Some(tail) = block.tail {
                    msg.push_str(&format!(
                        "{}expr:{}\n",
                        indent(nesting + 1),
                        debug_expr(
                            db,
                            hir_file,
                            resolution_map,
                            scope_origin,
                            tail,
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
                    db,
                    hir_file,
                    resolution_map,
                    scope_origin,
                    *condition,
                    nesting,
                ));
                msg.push(' ');
                msg.push_str(&debug_expr(
                    db,
                    hir_file,
                    resolution_map,
                    scope_origin,
                    *then_branch,
                    nesting,
                ));

                if let Some(else_branch) = else_branch {
                    msg.push_str(" else ");
                    msg.push_str(&debug_expr(
                        db,
                        hir_file,
                        resolution_map,
                        scope_origin,
                        *else_branch,
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
                        &debug_expr(db, hir_file, resolution_map, scope_origin, *value, nesting,)
                    ));
                }

                msg
            }
            Expr::Loop { block } => {
                let mut msg = "loop".to_string();
                msg.push_str(&format!(
                    " {}",
                    &debug_expr(db, hir_file, resolution_map, scope_origin, *block, nesting)
                ));

                msg
            }
            Expr::Continue => "continue".to_string(),
            Expr::Break { value } => {
                let mut msg = "break".to_string();
                if let Some(value) = value {
                    msg.push_str(&format!(
                        " {}",
                        &debug_expr(db, hir_file, resolution_map, scope_origin, *value, nesting,)
                    ));
                }

                msg
            }
            Expr::Missing => "<missing>".to_string(),
        }
    }

    fn debug_symbol(
        db: &dyn HirMasterDatabase,
        hir_file: &HirFile,
        resolution_map: &ResolutionMap,
        scope_origin: ModuleScopeOrigin,
        symbol: &Symbol,
        nesting: usize,
    ) -> String {
        match &symbol {
            Symbol::Local { name, expr } => match expr.lookup(hir_file.db(db)) {
                Expr::Symbol { .. }
                | Expr::Binary { .. }
                | Expr::Missing
                | Expr::Literal(_)
                | Expr::Unary { .. }
                | Expr::Call { .. }
                | Expr::If { .. }
                | Expr::Return { .. }
                | Expr::Loop { .. }
                | Expr::Continue
                | Expr::Break { .. } => {
                    let expr_text =
                        debug_expr(db, hir_file, resolution_map, scope_origin, *expr, nesting);
                    format!("${}:{}", name.text(db).to_string(), expr_text)
                }
                Expr::Block { .. } => name.text(db).to_string(),
            },
            Symbol::Param { name, .. } => {
                let name = name.text(db);
                format!("param:{name}")
            }
            Symbol::Missing { path } => {
                let resolving_status = resolution_map.item_by_symbol(path).unwrap();
                debug_resolution_status(db, resolving_status, resolution_map)
            }
        }
    }

    fn debug_resolution_status(
        db: &dyn HirMasterDatabase,
        resolution_status: ResolutionStatus,
        _resolution_map: &ResolutionMap,
    ) -> String {
        match resolution_status {
            ResolutionStatus::Unresolved => "<unknown>".to_string(),
            ResolutionStatus::Error => "<missing>".to_string(),
            ResolutionStatus::Resolved { path, item } => {
                let path = debug_path(db, &path);
                match item {
                    Item::Function(_) => {
                        format!("fn:{path}")
                    }
                    Item::Module(_) => {
                        format!("mod:{path}")
                    }
                    Item::UseItem(_) => {
                        unreachable!()
                    }
                }
            }
        }
    }

    fn debug_path(db: &dyn HirMasterDatabase, path: &Path) -> String {
        path.segments(db)
            .iter()
            .map(|segment| segment.text(db).to_string())
            .collect::<Vec<_>>()
            .join("::")
    }

    /// ルートファイル前提としてパースして、Podの期待結果をテストする
    fn check_in_root_file(fixture: &str, expected: Expect) {
        let mut fixture = fixture.to_string();
        fixture.insert_str(0, "//- /main.nail\n");

        check_pod_start_with_root_file(&fixture, expected);
    }

    /// ルートファイルからパースして、Podの期待結果をテストする
    fn check_pod_start_with_root_file(fixture: &str, expected: Expect) {
        let db = TestingDatabase::default();
        let mut source_db = FixtureDatabase::new(&db, fixture);

        let pods = parse_pods(&db, &mut source_db);

        expected.assert_eq(&debug_pods(&db, &pods));
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
                fn entry:main() -> () {
                    expr:10 - <missing>
                }
            "#]],
        );
    }

    #[test]
    fn lower_binary_expr_assign() {
        check_in_root_file(
            r#"
                fn main() {
                    let a = 0;
                    a = 1;
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    let a = 0
                    $a:0 = 1;
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
                fn entry:main() -> () {
                    let foo = 10
                    expr:$foo:10
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
                fn entry:main() -> () {
                    let a = 10
                    {
                        let b = 20
                        expr:{
                            let c = 30
                            expr:$a:10 + $b:20 + $c:30
                        }
                    }
                    expr:$a:10
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
                //- /main.nail
                fn entry:main() -> () {
                    let a = 10
                    $a:10
                    let a = 20
                    expr:$a:20
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
                //- /main.nail
                fn entry:main() -> () {
                    <missing>
                    let a = 10
                    $a:10
                    {
                        $a:10
                        let a = 20
                        {
                            $a:20
                            let a = 30
                            expr:$a:30
                        }
                        expr:$a:20
                    }
                    expr:$a:10
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
                //- /main.nail
                fn entry:main() -> () {
                    let a = {
                        <missing>
                        let a = 10
                        expr:$a:10
                    }
                    a
                    expr:20 + {
                        let b = 30
                        expr:a + $b:30
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
                //- /main.nail
                fn entry:main() -> () {
                    expr:{
                        let a = 10
                        expr:$a:10
                    } + {
                        let b = 30
                        expr:<missing> + $b:30
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
                //- /main.nail
                fn entry:main() -> () {
                    expr:{
                        let a = 10
                        expr:{
                            expr:$a:10
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
                //- /main.nail
                fn entry:main() -> () {
                    expr:-{
                        let a = 10
                        expr:-{
                            expr:-$a:10
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
                //- /main.nail
                fn foo() -> () {
                    let a = 10
                    expr:$a:10
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
                fn entry:main() -> () {
                    fn foo(a: <unknown>, b: <unknown>) -> () {
                        let a = 0
                        $a:0
                        param:b
                        fn bar(a: <unknown>, b: <unknown>) -> () {
                            expr:{
                                expr:param:a + param:b
                            }
                        }
                        $a:0
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
                //- /main.nail
                fn entry:main() -> () {
                    let a = 10
                    fn foo() -> () {
                        <missing>
                        let a = 20
                        expr:$a:20
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
                //- /main.nail
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
                                    expr:<missing> + <missing> + $c:20
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
                //- /main.nail
                fn entry:main() -> () {
                    <missing>
                    let a = 10
                    $a:10
                    fn foo() -> () {
                        <missing>
                        let a = 20
                        $a:20
                        {
                            $a:20
                            let a = 30
                            expr:$a:30
                        }
                        $a:20
                        fn bar() -> () {
                            <missing>
                            let a = 40
                            expr:$a:40
                        }
                        expr:$a:20
                    }
                    expr:$a:10
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
                //- /main.nail
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
                //- /main.nail
                fn entry:main() -> () {
                    expr:<missing> / {
                    }
                }
            "#]],
        );
    }

    #[test]
    fn function_missing_param() {
        check_in_root_file(
            "fn a(a, ) {}",
            expect![[r#"
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
    }

    #[test]
    fn function_missing_name() {
        check_in_root_file(
            "fn (a, ) {}",
            expect![[r#"
                //- /main.nail
                error: Undefined entry point.(help: fn main() { ... })
            "#]],
        );
    }

    #[test]
    fn call() {
        check_in_root_file(
            "fn main() { a() }",
            expect![[r#"
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
                fn entry:main() -> () {
                    fn a() -> () {
                        expr:10
                    }
                    let b = 20
                    expr:fn:a($b:20, 30)
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
    fn loop_expr() {
        check_in_root_file(
            r#"
                fn main() {
                    loop {
                        10;
                        20;
                    };
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    loop {
                        10;
                        20;
                    };
                }
            "#]],
        );
    }

    #[test]
    fn loop_expr_no_block() {
        check_in_root_file(
            r#"
                fn main() {
                    loop
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    expr:<missing>
                }
            "#]],
        );
    }

    #[test]
    fn continue_in_loop() {
        check_in_root_file(
            r#"
                fn main() {
                    loop {
                        continue;
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    expr:loop {
                        continue;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn break_in_loop() {
        check_in_root_file(
            r#"
                fn main() {
                    loop {
                        break;
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    expr:loop {
                        break;
                    }
                }
            "#]],
        );
    }

    #[test]
    fn while_expr() {
        check_in_root_file(
            r#"
                fn main() {
                    let a = 1;
                    while a < 3 {
                        a = a + 1;
                    };
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    let a = 1
                    loop {
                        if $a:1 < 3 {
                            $a:1 = $a:1 + 1;
                        } else {
                            break;
                        };
                    };
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
                //- /main.nail
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
                //- /main.nail
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
                //- /main.nail
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
    fn outline_module() {
        check_pod_start_with_root_file(
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
                    fn:mod_aaa::fn_aaa();
                }
                //- /mod_aaa.nail
                fn fn_aaa() -> () {
                }
            "#]],
        );
    }

    #[test]
    fn nested_outline_module() {
        check_pod_start_with_root_file(
            r#"
                //- /main.nail
                mod mod_aaa;

                fn main() {
                    mod_aaa::fn_aaa();
                    mod_aaa::mod_bbb::fn_bbb();
                }

                //- /mod_aaa.nail
                mod mod_bbb;
                fn fn_aaa() {
                    mod_bbb::fn_bbb();
                }

                //- /mod_aaa/mod_bbb.nail
                fn fn_bbb() {
                }
            "#,
            expect![[r#"
                //- /main.nail
                mod mod_aaa;
                fn entry:main() -> () {
                    fn:mod_aaa::fn_aaa();
                    fn:mod_aaa::mod_bbb::fn_bbb();
                }
                //- /mod_aaa.nail
                mod mod_bbb;
                fn fn_aaa() -> () {
                    fn:mod_bbb::fn_bbb();
                }
                //- /mod_aaa/mod_bbb.nail
                fn fn_bbb() -> () {
                }
            "#]],
        );
    }
}
