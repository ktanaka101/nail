//! 名前解決を行うモジュールです。
//!
//! 名前解決時に以下も行います。
//! - モジュールの構造を解析する
//! - パスをフルパスに脱糖する
//!
//! モジュールのスコープを木構造で構築する必要があります。
//! モジュールはrootから末端方向へ辿れる必要がありますが、途中から末端へと辿れる必要があります。
//! `super`の親を参照する使用があるため、あるモジュールのスコープから親へ辿れる必要があります。
//!
//! モジュールのスコープを途中から参照するには、参照部分からスコープへの対応表が必要です。
//! 参照部分には以下があります。
//! - 使用宣言(UseItem)
//! - パス(Path)
//! - 識別子(Ident)
//!
//! スコープを作成してから名前解決を行いたいため、上記の参照部分とスコープとの対応表を作成します。
//! この対応表は、スコープを作成する際に一緒に作成する必要があります。

mod module_scope;

use std::collections::HashMap;

pub use module_scope::{ModuleScope, ModuleScopeOrigin};

use self::module_scope::{ModuleScopes, ModuleScopesBuilder, NameResolutionCollection, PathMap};
use crate::{
    Function, HirMasterDatabase, Item, Name, NameSolutionPath, Path, Pod, Struct, Symbol, UseItem,
};

/// 名前解決を行います。
pub(crate) fn resolve_symbols(db: &dyn HirMasterDatabase, pod: &Pod) -> ResolutionMap {
    let module_scope_builder = ModuleScopesBuilder::new(db, pod);
    let (module_scopes, name_resolution_collection, path_map) = module_scope_builder.build();

    let name_resolver = NameResolver::new(&module_scopes, &name_resolution_collection, db);

    ResolutionMap {
        symbol_table: name_resolver.resolve(),
        path_map,
    }
}

/// 名前解決の結果を保持します。
#[derive(Debug)]
pub struct ResolutionMap {
    symbol_table: SymbolTable,
    path_map: PathMap,
}
impl ResolutionMap {
    /// シンボルの解決状態を取得します。
    pub fn item_by_symbol(&self, symbol: &NameSolutionPath) -> Option<ResolutionStatus> {
        self.symbol_table.item_by_symbol(symbol)
    }

    /// 使用宣言の解決状態を取得します。
    pub fn item_by_use_item(&self, use_item: &UseItem) -> Option<ResolutionStatus> {
        self.symbol_table.item_by_use_item(use_item)
    }

    /// 関数のフルパスを取得します。パス中に関数名は含まれません。
    pub fn path_of_function(&self, function: Function) -> Option<Path> {
        self.path_map.path_by_function(function)
    }

    /// 構造体のフルパスを取得します。パス中に構造体名は含まれません。
    pub fn path_of_struct(&self, struct_: Struct) -> Option<Path> {
        self.path_map.path_by_struct(struct_)
    }
}

#[derive(Debug)]
struct SymbolTable {
    item_by_use_item: HashMap<UseItem, ResolutionStatus>,
    item_by_symbol: HashMap<NameSolutionPath, ResolutionStatus>,
}
impl SymbolTable {
    fn new() -> Self {
        Self {
            item_by_use_item: HashMap::new(),
            item_by_symbol: HashMap::new(),
        }
    }

    fn insert_use_item(&mut self, use_item: UseItem, status: ResolutionStatus) {
        self.item_by_use_item.insert(use_item, status);
    }

    fn insert_symbol(&mut self, symbol: NameSolutionPath, status: ResolutionStatus) {
        self.item_by_symbol.insert(symbol, status);
    }

    fn item_by_symbol(&self, symbol: &NameSolutionPath) -> Option<ResolutionStatus> {
        self.item_by_symbol.get(symbol).cloned()
    }

    fn item_by_use_item(&self, use_item: &UseItem) -> Option<ResolutionStatus> {
        self.item_by_use_item.get(use_item).cloned()
    }
}

/// パスの中の区切られた部分の種類です。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum PathSegmentKind {
    /// 相対を表すパスセグメントです。
    ///
    /// 例: `aaa::bbb`
    ///      ^^^ 相対パスセグメント
    Relative(Name),
    /// Pod(パッケージ)を表すパスセグメントです。
    ///
    /// 例: `pod::io`
    ///      ^^^ Podパスセグメント
    Pod,
}

/// 解決状態を表します。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolutionStatus {
    /// 未解決の状態です。
    Unresolved,
    /// 解決済みの状態です。
    Resolved {
        /// 解決したパス
        path: Path,
        /// 解決したアイテム
        item: Item,
    },
    /// 解決できなかった状態です。
    Error,
}

struct NameResolver<'a> {
    module_scopes: &'a ModuleScopes,
    name_resolution_collection: &'a NameResolutionCollection,

    db: &'a dyn HirMasterDatabase,

    symbol_table: SymbolTable,
}

impl<'a> NameResolver<'a> {
    fn new(
        module_scopes: &'a ModuleScopes,
        name_resolution_collection: &'a NameResolutionCollection,
        db: &'a dyn HirMasterDatabase,
    ) -> Self {
        Self {
            module_scopes,
            name_resolution_collection,
            db,
            symbol_table: SymbolTable::new(),
        }
    }

    fn resolve(mut self) -> SymbolTable {
        // TODO: UseItemの一意性の見直し
        for use_item in self.name_resolution_collection.use_items() {
            self.symbol_table
                .insert_use_item(*use_item, ResolutionStatus::Unresolved);
        }

        for use_item in self.name_resolution_collection.use_items() {
            let path = use_item.path(self.db);
            let segments = self.resolve_path(path);

            let module_scope = self
                .module_scopes
                .module_scope_by_use_item(*use_item)
                .unwrap();

            let resolved_item = self.resolve_relative_item(module_scope, &segments);
            if let Some(resolved_item) = resolved_item {
                self.symbol_table.insert_use_item(
                    *use_item,
                    ResolutionStatus::Resolved {
                        path: *path,
                        item: resolved_item,
                    },
                );
            } else {
                self.symbol_table
                    .insert_use_item(*use_item, ResolutionStatus::Error);
            }
        }

        for symbol_in_scope in self.name_resolution_collection.symbols() {
            if let Symbol::Missing { path } = &symbol_in_scope.symbol {
                let module_scope = self
                    .module_scopes
                    .module_scope_by_origin(symbol_in_scope.scope_origin)
                    .unwrap();

                let segments = self.resolve_path(&path.path(self.db));

                let resolved_item = self.resolve_relative_item(module_scope, &segments);
                if let Some(resolved_item) = resolved_item {
                    self.symbol_table.insert_symbol(
                        *path,
                        ResolutionStatus::Resolved {
                            path: path.path(self.db),
                            item: resolved_item,
                        },
                    );
                } else {
                    self.symbol_table
                        .insert_symbol(*path, ResolutionStatus::Error);
                }
            }
        }

        self.symbol_table
    }

    fn resolve_relative_item(
        &self,
        module_scope: &ModuleScope,
        segments: &[PathSegmentKind],
    ) -> Option<Item> {
        match segments {
            [first] => match first {
                PathSegmentKind::Relative(first) => {
                    module_scope.lookup_item(self.module_scopes.ref_storage(), *first)
                }
                PathSegmentKind::Pod => unimplemented!(),
            },
            [first, rest @ ..] => match first {
                PathSegmentKind::Relative(first) => {
                    let item =
                        module_scope.lookup_item(self.module_scopes.ref_storage(), *first)?;
                    match item {
                        Item::Module(module) => {
                            let module_scope = self
                                .module_scopes
                                .module_scope_by_origin(ModuleScopeOrigin::Module {
                                    origin: module,
                                })
                                .unwrap();
                            self.resolve_relative_item_in_current_scope(module_scope, rest)
                        }
                        Item::Function(_) => {
                            // TODO: 関数内のスコープは外部から参照できないエラーを返す
                            None
                        }
                        Item::Struct(_) => {
                            // TODO: 構造体内のスコープは外部から参照できないエラーを返す
                            None
                        }
                        Item::UseItem(_) => unimplemented!(),
                    }
                }
                PathSegmentKind::Pod => unimplemented!(),
            },
            _ => None,
        }
    }

    fn resolve_relative_item_in_current_scope(
        &self,
        module_scope: &ModuleScope,
        segments: &[PathSegmentKind],
    ) -> Option<Item> {
        match segments {
            [first] => match first {
                PathSegmentKind::Relative(first) => {
                    module_scope.lookup_item_from_only_current(*first)
                }
                PathSegmentKind::Pod => unimplemented!(),
            },
            [first, rest @ ..] => match first {
                PathSegmentKind::Relative(first) => {
                    let item = module_scope.lookup_item_from_only_current(*first)?;
                    match item {
                        Item::Module(module) => {
                            let module_scope = self
                                .module_scopes
                                .module_scope_by_origin(ModuleScopeOrigin::Module {
                                    origin: module,
                                })
                                .unwrap();
                            self.resolve_relative_item_in_current_scope(module_scope, rest)
                        }
                        Item::Function(_) => {
                            // TODO: 関数内のスコープは外部から参照できないエラーを返す
                            None
                        }
                        Item::Struct(_) => {
                            // TODO: 構造体内のスコープは外部から参照できないエラーを返す
                            None
                        }
                        Item::UseItem(_) => unimplemented!(),
                    }
                }
                PathSegmentKind::Pod => unimplemented!(),
            },
            _ => None,
        }
    }

    fn resolve_path(&self, path: &Path) -> Vec<PathSegmentKind> {
        path.segments(self.db)
            .iter()
            .map(|segment| {
                let name = segment.text(self.db);
                match name.as_str() {
                    "pod" => PathSegmentKind::Pod,
                    _ => PathSegmentKind::Relative(*segment),
                }
            })
            .collect()
    }
}
