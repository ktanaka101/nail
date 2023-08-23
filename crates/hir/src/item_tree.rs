mod item;
mod item_scope;

use std::collections::HashMap;

pub use item::{Function, ItemDefId, Module, ModuleKind, Param, Type, UseItem};
pub use item_scope::{ItemScope, ParentScope};

use crate::{
    db::{Database, ItemScopeId, ModuleId, UseItemId},
    AstId, NailFile, Name, Path,
};

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

/// `ItemTree`は、ファイル内のすべてのアイテムをツリー構造で保持するデータ構造です。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    /// ファイル
    pub file: NailFile,
    /// ファイル内のトップレベルのアイテムスコープID
    pub top_level_scope: ItemScopeId,
    /// ItemTree内の関数一覧
    functions: Vec<Function>,
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
    scope_by_module: HashMap<ModuleId, ItemScopeId>,
    /// モジュール(AST)に対応するモジュールID
    module_by_ast_module: HashMap<ModuleAstId, ModuleId>,

    /// 使用宣言(AST)に対応する使用宣言ID
    use_item_by_ast_use: HashMap<UseAstId, UseItemId>,
}
impl ItemTree {
    /// トップレベルのアイテムスコープを取得します。
    /// トップレベルのアイテムスコープは1ファイルに必ず存在します。
    pub fn top_level_scope<'a>(&self, db: &'a Database) -> &'a ItemScope {
        self.top_level_scope.lookup(db)
    }

    /// ItemTree内の関数一覧を返す
    pub fn functions(&self) -> &[Function] {
        &self.functions
    }

    /// ブロック(AST)に対応するアイテムスコープIDを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn scope_id_by_block(&self, ast: &BlockAstId) -> Option<ItemScopeId> {
        self.scope_by_block.get(ast).copied()
    }

    /// ブロック(AST)に対応するアイテムスコープを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn scope_by_block<'a>(&self, db: &'a Database, ast: &BlockAstId) -> Option<&'a ItemScope> {
        self.scope_id_by_block(ast)
            .map(|scope_id| scope_id.lookup(db))
    }

    /// モジュール(AST)に対応するアイテムスコープを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn scope_by_ast_module<'a>(
        &self,
        db: &'a Database,
        ast_id: &ModuleAstId,
    ) -> Option<&'a ItemScope> {
        let scope_id = self.scope_by_module_ast.get(ast_id);
        scope_id.map(|scope_id| scope_id.lookup(db))
    }

    /// モジュールに対応するアイテムスコープを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn scope_by_module<'a>(&self, db: &'a Database, id: &ModuleId) -> Option<&'a ItemScope> {
        let scope_id = self.scope_by_module.get(id);
        scope_id.map(|scope_id| scope_id.lookup(db))
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

    /// モジュール(AST)に対応するモジュールIDを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn module_id_by_ast_module(&self, module_ast_id: ModuleAstId) -> Option<ModuleId> {
        self.module_by_ast_module.get(&module_ast_id).copied()
    }

    /// モジュール(AST)に対応するモジュールを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn module_by_ast_module_id<'a>(
        &self,
        db: &'a Database,
        module_ast_id: ModuleAstId,
    ) -> Option<&'a Module> {
        self.module_id_by_ast_module(module_ast_id)
            .map(|module_id| module_id.lookup(db))
    }

    /// 使用宣言(AST)に対応する使用宣言IDを取得します。
    /// 存在しない場合は`None`を返します。
    pub fn use_item_id_by_ast_use(&self, use_ast_id: UseAstId) -> Option<UseItemId> {
        self.use_item_by_ast_use.get(&use_ast_id).copied()
    }

    /// 使用宣言(AST)に対応する使用宣言を取得します。
    /// 存在しない場合は`None`を返します。
    pub fn use_item_by_ast_use<'a>(
        &self,
        db: &'a Database,
        use_ast_id: UseAstId,
    ) -> Option<&'a UseItem> {
        self.use_item_id_by_ast_use(use_ast_id)
            .map(|use_item_id| use_item_id.lookup(db))
    }
}

/// アイテムツリー構築用コンテキスト
pub(crate) struct ItemTreeBuilderContext {
    file: NailFile,

    functions: Vec<Function>,

    scope_by_block: HashMap<BlockAstId, ItemScopeId>,
    block_by_scope: HashMap<ItemScopeId, BlockAstId>,
    function_by_block: HashMap<BlockAstId, Function>,
    block_by_function: HashMap<Function, BlockAstId>,

    scope_by_module_ast: HashMap<ModuleAstId, ItemScopeId>,
    scope_by_module: HashMap<ModuleId, ItemScopeId>,
    module_by_ast_module: HashMap<ModuleAstId, ModuleId>,

    use_item_by_ast_use: HashMap<UseAstId, UseItemId>,
}
impl ItemTreeBuilderContext {
    /// コンテキストを作成します。
    pub(crate) fn new(file: NailFile) -> Self {
        Self {
            file,
            functions: vec![],
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
        let top_level_scope_id = db.alloc_item_scope(top_level_scope);

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
            functions: self.functions,
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
    ) -> Option<ItemDefId> {
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
                        let param = Param { name, ty, pos };
                        db.alloc_param(param)
                    })
                    .collect::<Vec<_>>();
                let param_by_name = params
                    .iter()
                    .enumerate()
                    .filter_map(|param| {
                        let p = param.1.lookup(db);
                        p.name.map(|name| (name, *param.1))
                    })
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
                    current_scope.lookup(db).path(db),
                    name,
                    params,
                    param_by_name,
                    return_type,
                    ast_id,
                );
                self.functions.push(function);

                if let Some(name) = name {
                    current_scope.lookup_mut(db).insert_function(name, function);
                }

                let block = self.build_block(salsa_db, block, parent, db, name);
                self.function_by_block.insert(block.clone(), function);
                self.block_by_function.insert(function, block);

                Some(ItemDefId::Function(function))
            }
            ast::Item::Module(module) => {
                let module_id = self.build_module(salsa_db, &module, current_scope, parent, db)?;
                Some(ItemDefId::Module(module_id))
            }
            ast::Item::Use(r#use) => {
                let use_item_id = self.build_use(salsa_db, &r#use, current_scope, parent, db)?;
                Some(ItemDefId::UseItem(use_item_id))
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
        let scope_id = db.alloc_item_scope(scope);
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
    ) -> Option<ModuleId> {
        if let Some(name) = module.name() {
            let module_name = Name::new(salsa_db, name.name().to_string());

            let scope = ItemScope::new_with_name(Some(parent), module_name);
            let scope_id = db.alloc_item_scope(scope);

            let current = ParentScope::new(scope_id);

            let module_ast_id = db.alloc_node(module, self.file);
            self.scope_by_module_ast
                .insert(module_ast_id.clone(), scope_id);

            let hir_module = if let Some(item_list) = module.items() {
                let mut items = vec![];
                for item in item_list.items() {
                    if let Some(item) =
                        self.build_item(salsa_db, item, scope_id, current.clone(), db)
                    {
                        items.push(item);
                    }
                }
                Module {
                    name: module_name,
                    kind: ModuleKind::Inline { items },
                }
            } else {
                Module {
                    name: module_name,
                    kind: ModuleKind::Outline,
                }
            };

            let module_id = db.alloc_module(hir_module);
            current_scope
                .lookup_mut(db)
                .insert_module(module_name, module_id);
            self.module_by_ast_module.insert(module_ast_id, module_id);
            self.scope_by_module.insert(module_id, scope_id);

            Some(module_id)
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
    ) -> Option<UseItemId> {
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
                let use_item = UseItem {
                    path,
                    name,
                    item_scope: current_scope,
                };
                let use_item_id = db.alloc_use_item(use_item);
                current_scope
                    .lookup_mut(db)
                    .insert_use_item(name, use_item_id);

                let use_item_ast_id = db.alloc_node(r#use, self.file);
                self.use_item_by_ast_use
                    .insert(use_item_ast_id, use_item_id);

                Some(use_item_id)
            }
        }
    }
}
