mod item;
mod item_scope;

use std::collections::HashMap;

pub use item::{Function, Item, Module, ModuleKind, Param, Type, UseItem};
pub use item_scope::{ItemScope, ParentScope};
use la_arena::{Arena, Idx};

use crate::{db::Database, AstId, NailFile, Name, Path};

/// `BlockAstId`は`ast::BlockExpr`（ブロック式）ノードを一意に識別するためのAST IDです。
/// この型は、ファイル内の特定のブロック式ノードを一意に参照するために使用されます。
/// `AstId`構造体のジェネリクスとして`ast::BlockExpr`を指定することで、型レベルでブロック式ノードのIDとして機能します。
type BlockAstId = AstId<ast::BlockExpr>;

/// `ModuleAstId`は`ast::Module`（モジュール）ノードを一意に識別するためのAST IDです。
/// この型は、ファイル内の特定のモジュールノードを一意に参照するために使用されます。
type ModuleAstId = AstId<ast::Module>;

/// `UseAstId`は`ast::Use`（使用宣言）ノードを一意に識別するためのAST IDです。
/// この型は、ファイル内の特定の使用宣言ノードを一意に参照するために使用されます。
type UseAstId = AstId<ast::Use>;

/// アイテムスコープを一意に特定するためのIDです。
/// 元データは`Database`に格納されています。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemScopeId(Idx<ItemScope>);
impl ItemScopeId {
    /// DBからアイテムスコープを取得します。
    pub fn lookup(self, item_tree: &ItemTree) -> &ItemScope {
        &item_tree.item_scopes[self.0]
    }
}

/// `ItemTree`は、ファイル内のすべてのアイテムをツリー構造で保持するデータ構造です。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    /// ファイル
    pub file: NailFile,
    /// ファイル内のトップレベルのアイテムスコープID
    pub top_level_scope: ItemScopeId,
    /// ItemTree内のアイテムスコープ一覧
    item_scopes: Arena<ItemScope>,
    /// ItemTree内の関数一覧
    functions: Vec<Function>,
    /// ItemTree内のモジュール一覧
    modules: Vec<Module>,
    /// ItemTree内の使用宣言一覧
    use_items: Vec<UseItem>,
    /// ブロック(AST)に対応するアイテムスコープID
    /// トップレベルのアイテムスコープは含まれません。
    scope_by_block: HashMap<BlockAstId, ItemScopeId>,
    /// アイテムスコープに対応するブロックID
    /// トップレベルのアイテムスコープは含まれません。
    block_by_scope: HashMap<ItemScopeId, BlockAstId>,
    /// ブロック(AST)に対応する関数ID
    function_by_block: HashMap<BlockAstId, Function>,
    /// 関数に対応するブロックID
    block_by_function: HashMap<Function, BlockAstId>,

    /// モジュール(AST)に対応するアイテムスコープID
    scope_by_module_ast: HashMap<ModuleAstId, ItemScopeId>,
    /// モジュールに対応するアイテムスコープID
    scope_by_module: HashMap<Module, ItemScopeId>,
    /// モジュール(AST)に対応するモジュールID
    module_by_ast_module: HashMap<ModuleAstId, Module>,

    /// 使用宣言(AST)に対応する使用宣言ID
    use_item_by_ast_use: HashMap<UseAstId, UseItem>,
}
impl ItemTree {
    /// トップレベルのアイテムスコープを取得します。
    /// トップレベルのアイテムスコープは1ファイルに必ず存在します。
    pub fn top_level_scope(&self) -> &ItemScope {
        self.top_level_scope.lookup(self)
    }

    /// データベースに格納されているアイテムスコープ一覧を返します。
    pub fn item_scopes(&self) -> impl Iterator<Item = (ItemScopeId, &ItemScope)> {
        self.item_scopes
            .iter()
            .map(|(idx, item_scope)| (ItemScopeId(idx), item_scope))
    }

    /// ItemTree内の関数一覧を返す
    pub fn functions(&self) -> &[Function] {
        &self.functions
    }

    /// ItemTree内の関数一覧を返す
    pub fn modules(&self) -> &[Module] {
        &self.modules
    }

    /// ブロック(AST)に対応するアイテムスコープIDを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn scope_id_by_block(&self, ast: &BlockAstId) -> Option<ItemScopeId> {
        self.scope_by_block.get(ast).copied()
    }

    /// ブロック(AST)に対応するアイテムスコープを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn scope_by_block<'a>(&'a self, ast: &BlockAstId) -> Option<&'a ItemScope> {
        self.scope_id_by_block(ast)
            .map(|scope_id| scope_id.lookup(self))
    }

    /// モジュール(AST)に対応するアイテムスコープを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn scope_by_ast_module<'a>(&'a self, ast_id: &ModuleAstId) -> Option<&'a ItemScope> {
        let scope_id = self.scope_by_module_ast.get(ast_id);
        scope_id.map(|scope_id| scope_id.lookup(self))
    }

    /// モジュールに対応するアイテムスコープを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn scope_by_module<'a>(&'a self, id: &Module) -> Option<&'a ItemScope> {
        let scope_id = self.scope_by_module.get(id);
        scope_id.map(|scope_id| scope_id.lookup(self))
    }

    /// ブロック(AST)に対応する関数を取得します。
    /// 存在しない場合は`None`を返します。
    pub fn function_by_block(&self, block_ast_id: &BlockAstId) -> Option<Function> {
        self.function_by_block.get(block_ast_id).copied()
    }

    /// 関数に対応するブロック(AST)IDを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn block_id_by_function(&self, function: &Function) -> Option<BlockAstId> {
        Some(self.block_by_function.get(function)?.clone())
    }

    /// モジュール(AST)に対応するモジュールを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn module_by_ast_module(&self, module_ast_id: ModuleAstId) -> Option<Module> {
        self.module_by_ast_module.get(&module_ast_id).copied()
    }

    /// 使用宣言(AST)に対応する使用宣言IDを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn use_item_by_ast_use(&self, use_ast_id: UseAstId) -> Option<UseItem> {
        self.use_item_by_ast_use.get(&use_ast_id).copied()
    }
}

impl ItemScopeId {
    fn lookup_from_arena<'a>(&self, arena: &'a Arena<ItemScope>) -> &'a ItemScope {
        &arena[self.0]
    }
}

/// アイテムツリー構築用コンテキスト
pub(crate) struct ItemTreeBuilderContext {
    file: NailFile,

    item_scopes: Arena<ItemScope>,

    functions: Vec<Function>,
    modules: Vec<Module>,
    use_items: Vec<UseItem>,

    scope_by_block: HashMap<BlockAstId, ItemScopeId>,
    block_by_scope: HashMap<ItemScopeId, BlockAstId>,
    function_by_block: HashMap<BlockAstId, Function>,
    block_by_function: HashMap<Function, BlockAstId>,

    scope_by_module_ast: HashMap<ModuleAstId, ItemScopeId>,
    scope_by_module: HashMap<Module, ItemScopeId>,
    module_by_ast_module: HashMap<ModuleAstId, Module>,

    use_item_by_ast_use: HashMap<UseAstId, UseItem>,
}
impl ItemTreeBuilderContext {
    /// コンテキストを作成します。
    pub(crate) fn new(file: NailFile) -> Self {
        Self {
            file,
            item_scopes: Arena::new(),
            functions: vec![],
            modules: vec![],
            use_items: vec![],
            scope_by_block: HashMap::new(),
            block_by_scope: HashMap::new(),
            function_by_block: HashMap::new(),
            block_by_function: HashMap::new(),
            scope_by_module_ast: HashMap::new(),
            scope_by_module: HashMap::new(),
            module_by_ast_module: HashMap::new(),
            use_item_by_ast_use: HashMap::new(),
        }
    }

    /// アイテムツリーを構築します。
    pub(crate) fn build(
        mut self,
        salsa_db: &dyn crate::Db,
        ast: &ast::SourceFile,
        db: &mut Database,
    ) -> ItemTree {
        let top_level_scope = ItemScope::new_with_nameless(None);
        let top_level_scope_id = self.alloc_item_scope(top_level_scope);

        for item in ast.items() {
            self.build_item(
                salsa_db,
                item,
                top_level_scope_id,
                ParentScope::new(top_level_scope_id),
                db,
            );
        }

        ItemTree {
            file: self.file,
            top_level_scope: top_level_scope_id,
            item_scopes: self.item_scopes,
            functions: self.functions,
            modules: self.modules,
            use_items: self.use_items,
            scope_by_block: self.scope_by_block,
            block_by_scope: self.block_by_scope,
            function_by_block: self.function_by_block,
            block_by_function: self.block_by_function,
            scope_by_module_ast: self.scope_by_module_ast,
            scope_by_module: self.scope_by_module,
            module_by_ast_module: self.module_by_ast_module,
            use_item_by_ast_use: self.use_item_by_ast_use,
        }
    }

    /// アイテムを構築します。
    /// アイテムが構築できなかった場合は`None`を返します。
    fn build_item(
        &mut self,
        salsa_db: &dyn crate::Db,
        item: ast::Item,
        current_scope: ItemScopeId,
        parent: ParentScope,
        db: &mut Database,
    ) -> Option<Item> {
        match item {
            ast::Item::FunctionDef(def) => {
                let block = def.body()?;
                let params = def
                    .params()?
                    .params()
                    .enumerate()
                    .map(|(pos, param)| {
                        let name = param
                            .name()
                            .map(|name| Name::new(salsa_db, name.name().to_string()));
                        let ty = self.lower_ty(param.ty());
                        Param::new(salsa_db, name, ty, pos)
                    })
                    .collect::<Vec<_>>();
                let param_by_name = params
                    .iter()
                    .filter_map(|param| param.name(salsa_db).map(|name| (name, *param)))
                    .collect::<HashMap<_, _>>();
                let return_type = if let Some(return_type) = def.return_type() {
                    self.lower_ty(return_type.ty())
                } else {
                    Type::Unit
                };

                let name = def
                    .name()
                    .map(|name| Name::new(salsa_db, name.name().to_string()));

                let ast_id = db.alloc_node(&def, self.file);
                let function = Function::new(
                    salsa_db,
                    self.lookup_item_scope(current_scope)
                        .path(&self.item_scopes),
                    name,
                    params,
                    param_by_name,
                    return_type,
                    ast_id,
                );
                self.functions.push(function);

                if let Some(name) = name {
                    self.lookup_mut_item_scope(current_scope)
                        .insert_function(name, function);
                }

                let block = self.build_block(salsa_db, block, parent, db, name);
                self.function_by_block.insert(block.clone(), function);
                self.block_by_function.insert(function, block);

                Some(Item::Function(function))
            }
            ast::Item::Module(ast_module) => {
                let module = self.build_module(salsa_db, &ast_module, current_scope, parent, db)?;
                Some(Item::Module(module))
            }
            ast::Item::Use(r#use) => {
                let use_item = self.build_use(salsa_db, &r#use, current_scope, parent, db)?;
                Some(Item::UseItem(use_item))
            }
        }
    }

    /// ステートメントを構築します。
    /// ステートメントが構築できなかった場合は`None`を返します。
    fn build_stmt(
        &mut self,
        salsa_db: &dyn crate::Db,
        stmt: ast::Stmt,
        current_scope: ItemScopeId,
        parent: ParentScope,
        db: &mut Database,
    ) -> Option<()> {
        match stmt {
            ast::Stmt::ExprStmt(expr_stmt) => {
                self.build_expr(salsa_db, expr_stmt.expr()?, current_scope, parent, db)?
            }
            ast::Stmt::VariableDef(def) => {
                self.build_expr(salsa_db, def.value()?, current_scope, parent, db)?
            }
            ast::Stmt::Item(item) => {
                self.build_item(salsa_db, item, current_scope, parent, db)?;
            }
        }

        Some(())
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

    /// 式を構築します。
    /// 式自体はアイテムではないため、値を返すわけではありません。
    /// 式の中のアイテムを探すためにトラバースしています。
    /// 式の中のアイテムを構築できなかった場合は`None`を返します。
    /// 式の中にアイテムがあるのは、例えばブロック自体やIf式です。
    /// ```nail
    /// if true {
    ///     fn a() -> int { // Item
    ///         10
    ///     }
    /// }
    /// ```
    fn build_expr(
        &mut self,
        salsa_db: &dyn crate::Db,
        expr: ast::Expr,
        _current_scope: ItemScopeId,
        parent: ParentScope,
        db: &mut Database,
    ) -> Option<()> {
        match expr {
            ast::Expr::BinaryExpr(binary) => {
                self.build_expr(salsa_db, binary.lhs()?, _current_scope, parent.clone(), db)?;
                self.build_expr(salsa_db, binary.rhs()?, _current_scope, parent, db)?;
            }
            ast::Expr::ParenExpr(paren) => {
                self.build_expr(salsa_db, paren.expr()?, _current_scope, parent, db)?;
            }
            ast::Expr::UnaryExpr(unary) => {
                self.build_expr(salsa_db, unary.expr()?, _current_scope, parent, db)?;
            }
            ast::Expr::BlockExpr(block) => {
                self.build_block(salsa_db, block, parent, db, None);
            }
            ast::Expr::IfExpr(if_expr) => {
                self.build_expr(
                    salsa_db,
                    if_expr.condition()?,
                    _current_scope,
                    parent.clone(),
                    db,
                )?;
                self.build_block(salsa_db, if_expr.then_branch()?, parent.clone(), db, None);
                self.build_block(salsa_db, if_expr.else_branch()?, parent, db, None);
            }
            _ => (),
        };

        Some(())
    }

    /// ブロックを構築します。
    fn build_block(
        &mut self,
        salsa_db: &dyn crate::Db,
        block: ast::BlockExpr,
        parent: ParentScope,
        db: &mut Database,
        name: Option<Name>,
    ) -> AstId<ast::BlockExpr> {
        let scope = if let Some(name) = name {
            ItemScope::new_with_name(Some(parent), name)
        } else {
            ItemScope::new_with_nameless(Some(parent))
        };
        let scope_id = self.alloc_item_scope(scope);
        let block_ast_id = db.alloc_node(&block, self.file);
        let current = ParentScope::new(scope_id);
        for stmt in block.stmts() {
            self.build_stmt(salsa_db, stmt, scope_id, current.clone(), db);
        }

        self.scope_by_block.insert(block_ast_id.clone(), scope_id);
        self.block_by_scope.insert(scope_id, block_ast_id.clone());

        block_ast_id
    }

    /// モジュールを構築します。
    /// モジュールを構築できなかった場合は`None`を返します。
    fn build_module(
        &mut self,
        salsa_db: &dyn crate::Db,
        module: &ast::Module,
        current_scope: ItemScopeId,
        parent: ParentScope,
        db: &mut Database,
    ) -> Option<Module> {
        if let Some(name) = module.name() {
            let module_name = Name::new(salsa_db, name.name().to_string());

            let scope = ItemScope::new_with_name(Some(parent), module_name);
            let scope_id = self.alloc_item_scope(scope);

            let current = ParentScope::new(scope_id);

            let module_ast_id = db.alloc_node(module, self.file);
            self.scope_by_module_ast
                .insert(module_ast_id.clone(), scope_id);

            let module_kind = if let Some(item_list) = module.items() {
                let mut items = vec![];
                for item in item_list.items() {
                    if let Some(item) =
                        self.build_item(salsa_db, item, scope_id, current.clone(), db)
                    {
                        items.push(item);
                    }
                }
                ModuleKind::Inline { items }
            } else {
                ModuleKind::Outline
            };
            let module = Module::new(salsa_db, module_name, module_kind);

            self.modules.push(module);
            self.lookup_mut_item_scope(current_scope)
                .insert_module(module_name, module);
            self.module_by_ast_module.insert(module_ast_id, module);
            self.scope_by_module.insert(module, scope_id);

            Some(module)
        } else {
            None
        }
    }

    /// アイテムを構築します。
    /// アイテムを構築できなかった場合は`None`を返します。
    fn build_use(
        &mut self,
        salsa_db: &dyn crate::Db,
        r#use: &ast::Use,
        current_scope: ItemScopeId,
        _parent: ParentScope,
        db: &mut Database,
    ) -> Option<UseItem> {
        let use_path = r#use.path()?.segments().collect::<Vec<_>>();
        match use_path.as_slice() {
            [] => unreachable!("use path should not be empty"),
            [path @ .., name] => {
                let name = Name::new(salsa_db, name.name().unwrap().name().to_string());
                let segments = path
                    .iter()
                    .map(|segment| Name::new(salsa_db, segment.name().unwrap().name().to_string()))
                    .collect::<Vec<_>>();
                let path = Path { segments };
                let use_item = UseItem::new(salsa_db, name, path, current_scope);

                self.use_items.push(use_item);
                self.lookup_mut_item_scope(current_scope)
                    .insert_use_item(name, use_item);
                let use_item_ast_id = db.alloc_node(r#use, self.file);
                self.use_item_by_ast_use.insert(use_item_ast_id, use_item);

                Some(use_item)
            }
        }
    }

    /// アイテムスコープの共有参照を取得します。
    fn lookup_item_scope(&self, item_scope_id: ItemScopeId) -> &ItemScope {
        &self.item_scopes[item_scope_id.0]
    }

    /// アイテムスコープの排他参照を取得します。
    fn lookup_mut_item_scope(&mut self, item_scope_id: ItemScopeId) -> &mut ItemScope {
        &mut self.item_scopes[item_scope_id.0]
    }

    /// アイテムスコープを保存します。
    /// 保存時にデータを取得するためのIDを生成し返します。
    fn alloc_item_scope(&mut self, item_scope: ItemScope) -> ItemScopeId {
        ItemScopeId(self.item_scopes.alloc(item_scope))
    }
}
