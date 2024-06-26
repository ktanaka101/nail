use std::collections::HashMap;

use la_arena::{Arena, Idx};

use crate::{
    Expr, ExprId, Function, HirFile, HirMasterDatabase, Item, Module, Name, Path, Pod, Stmt,
    Struct, Symbol, Type, UseItem,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleScopeIdx(Idx<ModuleScope>);

/// モジュールのスコープです。
/// 以下が含まれます。
/// - アイテム
#[derive(Debug)]
pub struct ModuleScope {
    /// モジュールスコープのパスです。
    ///
    /// 末端の名前は含まれません。名前は`origin`を参照してください。
    pub path: Path,

    /// モジュールスコープの種別です。
    pub kind: ModuleScopeKind,

    /// モジュールスコープの元となる要素です。
    pub origin: ModuleScopeOrigin,

    /// 使用宣言でスコープに追加されたアイテム一覧です。
    use_item_by_name: HashMap<Name, UseItem>,

    /// スコープ内で定義されているモジュール一覧です。
    defined_module_by_name: HashMap<Name, Module>,

    /// スコープ内で定義されている関数一覧です。
    defined_function_by_name: HashMap<Name, Function>,

    /// スコープ内で定義されている構造体一覧です。
    defined_struct_by_name: HashMap<Name, Struct>,

    /// 子のモジュールスコープ一覧です。
    ///
    /// 名前がないモジュールスコープは含まれません。
    children: HashMap<Name, ModuleScopeIdx>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleScopeKind {
    TopLevel,
    SubLevel { parent: ModuleScopeIdx },
}

/// モジュールスコープの元アイテム
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleScopeOrigin {
    /// Podのルートスコープであることを表します。
    Pod {
        /// Podの名前
        name: Name,
    },
    /// 関数のスコープであることを表します。
    Function {
        /// スコープ元の関数
        origin: Function,
    },
    /// モジュールのスコープであることを表します。
    Module {
        /// スコープ元のモジュール
        origin: Module,
    },
    /// ブロックのスコープであることを表します。
    Block {
        /// スコープ元のブロック
        origin: ExprId,
    },
}

impl ModuleScope {
    /// ルートスコープを作成します。
    /// ルートスコープはPod内のルートを表します。
    fn root(db: &dyn HirMasterDatabase, name: Name) -> Self {
        Self {
            path: Path::new(db, vec![]),
            kind: ModuleScopeKind::TopLevel,
            origin: ModuleScopeOrigin::Pod { name },
            use_item_by_name: HashMap::new(),
            defined_module_by_name: HashMap::new(),
            defined_function_by_name: HashMap::new(),
            defined_struct_by_name: HashMap::new(),
            children: HashMap::new(),
        }
    }

    /// サブスコープを作成します。
    /// サブスコープはモジュール内のスコープを表します。
    fn sub_scope_on_function(path: Path, parent: ModuleScopeIdx, origin: Function) -> Self {
        Self {
            path,
            kind: ModuleScopeKind::SubLevel { parent },
            origin: ModuleScopeOrigin::Function { origin },
            use_item_by_name: HashMap::new(),
            defined_module_by_name: HashMap::new(),
            defined_function_by_name: HashMap::new(),
            defined_struct_by_name: HashMap::new(),
            children: HashMap::new(),
        }
    }

    fn sub_scope_on_module(path: Path, parent: ModuleScopeIdx, origin: Module) -> Self {
        Self {
            path,
            kind: ModuleScopeKind::SubLevel { parent },
            origin: ModuleScopeOrigin::Module { origin },
            use_item_by_name: HashMap::new(),
            defined_module_by_name: HashMap::new(),
            defined_function_by_name: HashMap::new(),
            defined_struct_by_name: HashMap::new(),
            children: HashMap::new(),
        }
    }

    fn sub_scope_on_block(path: Path, parent: ModuleScopeIdx, origin: ExprId) -> Self {
        Self {
            path,
            kind: ModuleScopeKind::SubLevel { parent },
            origin: ModuleScopeOrigin::Block { origin },
            use_item_by_name: HashMap::new(),
            defined_module_by_name: HashMap::new(),
            defined_function_by_name: HashMap::new(),
            defined_struct_by_name: HashMap::new(),
            children: HashMap::new(),
        }
    }

    /// 現在のスコープからのみアイテムを検索します。
    pub fn lookup_item_from_only_current(&self, name: Name) -> Option<Item> {
        let item = self.use_item_by_name.get(&name);
        if let Some(item) = item {
            return Some(Item::UseItem(*item));
        }

        let item = self.defined_module_by_name.get(&name);
        if let Some(item) = item {
            return Some(Item::Module(*item));
        }

        let item = self.defined_function_by_name.get(&name);
        if let Some(item) = item {
            return Some(Item::Function(*item));
        }

        let item = self.defined_struct_by_name.get(&name);
        if let Some(item) = item {
            return Some(Item::Struct(*item));
        }

        None
    }

    /// 現在のスコープからアイテムを検索し、存在しない場合は親スコープから検索します。
    pub fn lookup_item(&self, storage: &ModuleScopeStorage, name: Name) -> Option<Item> {
        let item = self.lookup_item_from_only_current(name);
        if item.is_some() {
            return item;
        }

        match self.kind {
            ModuleScopeKind::TopLevel => None,
            ModuleScopeKind::SubLevel { parent } => {
                let parent = &storage.module_scopes[parent.0];
                parent.lookup_item(storage, name)
            }
        }
    }

    /// 現在のスコープに使用宣言アイテムを追加します
    fn insert_use_item(&mut self, name: Name, use_item: UseItem) {
        self.use_item_by_name.insert(name, use_item);
    }

    fn define_function(&mut self, name: Name, function: Function) {
        self.defined_function_by_name.insert(name, function);
    }

    fn define_struct(&mut self, name: Name, struct_: Struct) {
        self.defined_struct_by_name.insert(name, struct_);
    }

    fn define_module(&mut self, name: Name, module: Module) {
        self.defined_module_by_name.insert(name, module);
    }

    /// 現在のスコープに子スコープを追加します。
    fn push_child_scope(&mut self, name: Name, scope_idx: ModuleScopeIdx) {
        self.children.insert(name, scope_idx);
    }
}

#[derive(Debug)]
pub struct ModuleScopeStorage {
    module_scopes: Arena<ModuleScope>,
}
impl ModuleScopeStorage {
    fn new() -> Self {
        Self {
            module_scopes: Arena::new(),
        }
    }

    fn alloc(&mut self, module_scope: ModuleScope) -> ModuleScopeIdx {
        ModuleScopeIdx(self.module_scopes.alloc(module_scope))
    }

    fn insert_use_item(&mut self, module_scope_idx: ModuleScopeIdx, name: Name, use_item: UseItem) {
        let module_scope = &mut self.module_scopes[module_scope_idx.0];
        module_scope.insert_use_item(name, use_item);
    }
}

/// モジュールのスコープ構造です。
///
/// 1つのPodに1つ存在します。
#[derive(Debug)]
pub(crate) struct ModuleScopes {
    storage: ModuleScopeStorage,

    ref_map: RefMap,
}
impl ModuleScopes {
    pub fn ref_storage(&self) -> &ModuleScopeStorage {
        &self.storage
    }

    pub(crate) fn module_scope_by_use_item(&self, use_item: UseItem) -> Option<&ModuleScope> {
        let module_scope_idx = self.ref_map.scope_by_use_item.get(&use_item)?;
        Some(&self.storage.module_scopes[module_scope_idx.0])
    }

    pub(crate) fn module_scope_by_origin(&self, origin: ModuleScopeOrigin) -> Option<&ModuleScope> {
        let module_scope_idx = self.ref_map.scope_by_origin.get(&origin)?;
        Some(&self.storage.module_scopes[module_scope_idx.0])
    }
}

/// 名前解決対象/スコープ生成対象とモジュールスコープの対応表です。
#[derive(Debug)]
pub(crate) struct RefMap {
    scope_by_use_item: HashMap<UseItem, ModuleScopeIdx>,
    scope_by_origin: HashMap<ModuleScopeOrigin, ModuleScopeIdx>,
}
impl RefMap {
    fn new() -> Self {
        Self {
            scope_by_use_item: HashMap::new(),
            scope_by_origin: HashMap::new(),
        }
    }

    fn insert_use_item(&mut self, use_item: UseItem, scope_idx: ModuleScopeIdx) {
        self.scope_by_use_item.insert(use_item, scope_idx);
    }

    fn insert_scope_origin(&mut self, origin: ModuleScopeOrigin, scope_idx: ModuleScopeIdx) {
        self.scope_by_origin.insert(origin, scope_idx);
    }
}

/// スコープ元とシンボルのセットです。
///
/// シンボルはスコープ内で一意である必要があります。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolInScopeOrigin {
    pub scope_origin: ModuleScopeOrigin,
    pub symbol: Symbol,
}

/// 名前解決の対象を保持します。
#[derive(Debug)]
pub(crate) struct NameResolutionCollection {
    use_items: Vec<UseItem>,
    symbols: Vec<SymbolInScopeOrigin>,
}
impl NameResolutionCollection {
    fn new() -> Self {
        Self {
            use_items: Vec::new(),
            symbols: Vec::new(),
        }
    }

    /// 名前解決対象の使用宣言一覧を返します。
    pub(crate) fn use_items(&self) -> &[UseItem] {
        &self.use_items
    }

    /// 名前解決対象のシンボル一覧を返します。
    pub(crate) fn symbols(&self) -> &[SymbolInScopeOrigin] {
        &self.symbols
    }

    fn push_use_item(&mut self, use_item: UseItem) {
        self.use_items.push(use_item);
    }

    fn push_symbol(&mut self, symbol: SymbolInScopeOrigin) {
        self.symbols.push(symbol);
    }
}

/// パスとの対応表です。
#[derive(Debug)]
pub struct PathMap {
    path_by_function: HashMap<Function, Path>,
    path_by_struct: HashMap<Struct, Path>,
}
impl PathMap {
    fn new() -> Self {
        Self {
            path_by_function: HashMap::new(),
            path_by_struct: HashMap::new(),
        }
    }

    pub(crate) fn path_by_function(&self, function: Function) -> Option<Path> {
        self.path_by_function.get(&function).copied()
    }

    pub(crate) fn path_by_struct(&self, struct_: Struct) -> Option<Path> {
        self.path_by_struct.get(&struct_).copied()
    }

    fn insert_function_path(&mut self, function: Function, path: Path) {
        self.path_by_function.insert(function, path);
    }

    fn insert_struct_path(&mut self, struct_: Struct, path: Path) {
        self.path_by_struct.insert(struct_, path);
    }
}

pub(crate) struct ModuleScopesBuilder<'a> {
    storage: ModuleScopeStorage,

    ref_map: RefMap,
    name_resolution_collection: NameResolutionCollection,

    path_map: PathMap,

    db: &'a dyn HirMasterDatabase,
    pod: &'a Pod,
}
impl<'a> ModuleScopesBuilder<'a> {
    pub(crate) fn new(db: &'a dyn HirMasterDatabase, pod: &'a Pod) -> Self {
        Self {
            storage: ModuleScopeStorage::new(),
            ref_map: RefMap::new(),
            name_resolution_collection: NameResolutionCollection::new(),
            path_map: PathMap::new(),
            db,
            pod,
        }
    }

    pub(crate) fn build(mut self) -> (ModuleScopes, NameResolutionCollection, PathMap) {
        let top_level_scope_idx = self
            .storage
            .alloc(ModuleScope::root(self.db, self.pod.name));
        self.ref_map.insert_scope_origin(
            ModuleScopeOrigin::Pod {
                name: self.pod.name,
            },
            top_level_scope_idx,
        );

        for item in self.pod.root_hir_file.top_level_items(self.db) {
            self.build_item(self.pod.root_hir_file, top_level_scope_idx, *item);
        }

        (
            ModuleScopes {
                storage: self.storage,
                ref_map: self.ref_map,
            },
            self.name_resolution_collection,
            self.path_map,
        )
    }

    fn build_item(&mut self, hir_file: HirFile, current_scope_idx: ModuleScopeIdx, item: Item) {
        match item {
            crate::Item::Module(module) => {
                self.build_module(hir_file, current_scope_idx, module);
            }
            crate::Item::Function(function) => {
                self.build_function(hir_file, current_scope_idx, function);
            }
            crate::Item::Struct(struct_) => {
                self.build_struct(hir_file, current_scope_idx, struct_);
            }
            crate::Item::UseItem(use_item) => {
                self.register_use_item(current_scope_idx, use_item.name(self.db), use_item);
            }
        }
    }

    fn build_module(
        &mut self,
        hir_file: HirFile,
        current_scope_idx: ModuleScopeIdx,
        module: Module,
    ) {
        self.define_module(current_scope_idx, module);

        let current_scope_idx = self.crete_scope_on_module(self.db, current_scope_idx, module);

        match module.kind(self.db) {
            crate::ModuleKind::Inline { items } => {
                for item in items {
                    self.build_item(hir_file, current_scope_idx, *item);
                }
            }
            crate::ModuleKind::Outline => {
                let hir_file = self
                    .pod
                    .get_hir_file_by_module(&module)
                    .expect("Must exist at this point.");
                for item in hir_file.top_level_items(self.db) {
                    self.build_item(hir_file, current_scope_idx, *item);
                }
            }
        }
    }

    fn build_function(
        &mut self,
        hir_file: HirFile,
        current_scope_idx: ModuleScopeIdx,
        function: Function,
    ) {
        self.define_function(current_scope_idx, function);

        // 関数のパラメータと戻り値の型は関数外スコープとして解決する
        // なぜなら、関数内の型を呼び出し側から参照することはできないため
        for param in function.params(self.db) {
            let param = param.data(hir_file.db(self.db));
            self.register_type_if_needed(current_scope_idx, param.ty.clone());
        }
        self.register_type_if_needed(current_scope_idx, function.return_type(self.db));

        // 関数のスコープを作成
        let current_scope_idx = self.create_scope_on_function(self.db, current_scope_idx, function);
        self.path_map.insert_function_path(
            function,
            self.storage.module_scopes[current_scope_idx.0].path,
        );

        let Expr::Block(function_body) = function.body(self.db, hir_file).unwrap() else {
            panic!("No Block")
        };

        for stmt in &function_body.stmts {
            self.build_stmt(hir_file, current_scope_idx, stmt);
        }

        if let Some(tail) = function_body.tail {
            self.build_expr(hir_file, current_scope_idx, tail);
        }
    }

    fn build_struct(
        &mut self,
        _hir_file: HirFile,
        current_scope_idx: ModuleScopeIdx,
        struct_: Struct,
    ) {
        self.define_struct(current_scope_idx, struct_);

        let current_path = self.build_current_path(current_scope_idx);
        self.path_map.insert_struct_path(struct_, current_path);
    }

    fn build_stmt(&mut self, hir_file: HirFile, current_scope_idx: ModuleScopeIdx, stmt: &Stmt) {
        match stmt {
            crate::Stmt::Let {
                name: _,
                binding: _,
                value,
            } => {
                self.build_expr(hir_file, current_scope_idx, *value);
            }
            crate::Stmt::Expr {
                expr,
                has_semicolon: _,
            } => self.build_expr(hir_file, current_scope_idx, *expr),
            crate::Stmt::Item { item } => self.build_item(hir_file, current_scope_idx, *item),
        }
    }

    fn build_expr(&mut self, hir_file: HirFile, current_scope_idx: ModuleScopeIdx, expr: ExprId) {
        match expr.lookup(hir_file.db(self.db)) {
            Expr::Literal(_) | Expr::Missing => (),
            Expr::Symbol(symbol) => {
                self.register_symbol(current_scope_idx, symbol.clone());
            }
            Expr::Binary { op: _, lhs, rhs } => {
                self.build_expr(hir_file, current_scope_idx, *lhs);
                self.build_expr(hir_file, current_scope_idx, *rhs);
            }
            Expr::Unary { op: _, expr } => {
                self.build_expr(hir_file, current_scope_idx, *expr);
            }
            Expr::Call { callee, args } => {
                self.register_symbol(current_scope_idx, callee.clone());
                for arg in args {
                    self.build_expr(hir_file, current_scope_idx, *arg);
                }
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.build_expr(hir_file, current_scope_idx, *condition);
                self.build_expr(hir_file, current_scope_idx, *then_branch);
                if let Some(else_branch) = else_branch {
                    self.build_expr(hir_file, current_scope_idx, *else_branch);
                }
            }
            Expr::Return { value } => {
                if let Some(value) = value {
                    self.build_expr(hir_file, current_scope_idx, *value);
                }
            }
            Expr::Block(_block) => {
                self.build_block(hir_file, current_scope_idx, expr);
            }
            Expr::Loop { block } => self.build_expr(hir_file, current_scope_idx, *block),
            Expr::Continue => (),
            Expr::Break { value } => {
                if let Some(value) = value {
                    self.build_expr(hir_file, current_scope_idx, *value);
                }
            }
            Expr::Record { symbol, fields } => {
                self.register_symbol(current_scope_idx, symbol.clone());
                for field in fields {
                    self.build_expr(hir_file, current_scope_idx, field.value);
                }
            }
            Expr::Field { base, name: _ } => {
                self.build_expr(hir_file, current_scope_idx, *base);
            }
        }
    }

    fn build_block(&mut self, hir_file: HirFile, current_scope_idx: ModuleScopeIdx, block: ExprId) {
        let current_scope_idx = self.create_scope_on_block(current_scope_idx, block);

        let Expr::Block(block) = block.lookup(hir_file.db(self.db)) else {
            panic!("Should be block")
        };

        for stmt in &block.stmts {
            self.build_stmt(hir_file, current_scope_idx, stmt);
        }

        if let Some(tail) = &block.tail {
            self.build_expr(hir_file, current_scope_idx, *tail);
        }
    }

    /// モジュールのモジュールスコープを作成します。
    ///
    /// モジュールには名前があるため、現在のスコープに子スコープとしても参照を保存します。
    fn crete_scope_on_module(
        &mut self,
        db: &dyn HirMasterDatabase,
        current_scope_idx: ModuleScopeIdx,
        module: Module,
    ) -> ModuleScopeIdx {
        let current_path = self.build_current_path(current_scope_idx);
        let name = module.name(db);
        let child_scope_idx = self.storage.alloc(ModuleScope::sub_scope_on_module(
            current_path,
            current_scope_idx,
            module,
        ));
        self.storage.module_scopes[current_scope_idx.0].push_child_scope(name, child_scope_idx);

        self.ref_map.insert_scope_origin(
            ModuleScopeOrigin::Module { origin: module },
            child_scope_idx,
        );

        child_scope_idx
    }

    /// 関数ボディのモジュールスコープを作成します。
    ///
    /// 関数には名前があるため、現在のスコープに子スコープとしても参照を保存します。
    fn create_scope_on_function(
        &mut self,
        db: &dyn HirMasterDatabase,
        current_scope_idx: ModuleScopeIdx,
        function: Function,
    ) -> ModuleScopeIdx {
        let current_path = self.build_current_path(current_scope_idx);
        let name = function.name(db);
        let child_scope_idx = self.storage.alloc(ModuleScope::sub_scope_on_function(
            current_path,
            current_scope_idx,
            function,
        ));
        self.storage.module_scopes[current_scope_idx.0].push_child_scope(name, child_scope_idx);

        self.ref_map.insert_scope_origin(
            ModuleScopeOrigin::Function { origin: function },
            child_scope_idx,
        );

        child_scope_idx
    }

    /// ブロックのモジュールスコープを作成します。
    ///
    /// ブロックには名前がないため、現在のスコープに子スコープとしては参照を保存しません。
    fn create_scope_on_block(
        &mut self,
        current_scope_idx: ModuleScopeIdx,
        block: ExprId,
    ) -> ModuleScopeIdx {
        let current_path = self.build_current_path(current_scope_idx);
        let child_scope_idx = self.storage.alloc(ModuleScope::sub_scope_on_block(
            current_path,
            current_scope_idx,
            block,
        ));

        self.ref_map
            .insert_scope_origin(ModuleScopeOrigin::Block { origin: block }, child_scope_idx);

        child_scope_idx
    }

    /// モジュールスコープに関数を登録します。
    fn define_function(&mut self, current_scope_idx: ModuleScopeIdx, function: Function) {
        self.storage.module_scopes[current_scope_idx.0]
            .define_function(function.name(self.db), function);
    }

    /// モジュールスコープに構造体を登録します。
    fn define_struct(&mut self, current_scope_idx: ModuleScopeIdx, struct_: Struct) {
        self.storage.module_scopes[current_scope_idx.0]
            .define_struct(struct_.name(self.db), struct_);
    }

    /// モジュールスコープにモジュールを登録します。
    fn define_module(&mut self, current_scope_idx: ModuleScopeIdx, module: Module) {
        self.storage.module_scopes[current_scope_idx.0].define_module(module.name(self.db), module);
    }

    /// 名前解決対象の使用宣言を登録します。
    ///
    /// 使用宣言をInternし、
    /// 使用宣言に紐づくモジュールスコープとの対応表にも追加します。
    fn register_use_item(
        &mut self,
        module_scope_idx: ModuleScopeIdx,
        name: Name,
        use_item: UseItem,
    ) {
        self.storage
            .insert_use_item(module_scope_idx, name, use_item);
        self.ref_map.insert_use_item(use_item, module_scope_idx);
        self.name_resolution_collection.push_use_item(use_item);
    }

    /// 名前解決対象のシンボルを登録します。
    ///
    /// シンボルに紐づくモジュールスコープとの対応表に追加します。
    fn register_symbol(&mut self, module_scope_idx: ModuleScopeIdx, symbol: Symbol) {
        self.name_resolution_collection
            .push_symbol(SymbolInScopeOrigin {
                scope_origin: self.storage.module_scopes[module_scope_idx.0].origin,
                symbol,
            });
    }

    /// 型がパスの場合、その型を登録します。
    fn register_type_if_needed(&mut self, module_scope_idx: ModuleScopeIdx, ty: Type) {
        match ty {
            Type::Integer
            | Type::String
            | Type::Char
            | Type::Boolean
            | Type::Unit
            | Type::Unknown => (),
            Type::Custom(symbol) => {
                self.register_symbol(module_scope_idx, symbol);
            }
        }
    }

    /// 指定したモジュールスコープのパスを生成し返します。
    fn build_current_path(&self, current_scope_idx: ModuleScopeIdx) -> Path {
        let mut segments = Vec::new();
        let mut current_scope_idx = current_scope_idx;
        loop {
            let current_scope = &self.storage.module_scopes[current_scope_idx.0];
            let segment = match current_scope.origin {
                ModuleScopeOrigin::Pod { name } => name,
                ModuleScopeOrigin::Function { origin } => origin.name(self.db),
                ModuleScopeOrigin::Module { origin } => origin.name(self.db),
                ModuleScopeOrigin::Block { .. } => Name::new(self.db, "{block}".to_string()),
            };
            segments.push(segment);

            match current_scope.kind {
                ModuleScopeKind::TopLevel => break,
                ModuleScopeKind::SubLevel { parent } => {
                    current_scope_idx = parent;
                }
            }
        }
        segments.reverse();
        Path::new(self.db, segments)
    }
}
