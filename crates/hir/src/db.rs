use std::collections::HashMap;

use la_arena::{Arena, Idx};
use syntax::SyntaxNodePtr;

use crate::{
    item_tree::{ItemScope, UseItem},
    AstId, AstPtr, InFile, NailFile,
};

/// アイテムスコープを一意に特定するためのIDです。
/// 元データは`Database`に格納されています。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemScopeId(Idx<ItemScope>);
impl ItemScopeId {
    /// DBからアイテムスコープを取得します。
    pub fn lookup(self, db: &Database) -> &ItemScope {
        &db.item_scopes[self.0]
    }

    /// DBから可変なアイテムスコープを取得します。
    pub(crate) fn lookup_mut(self, db: &mut Database) -> &mut ItemScope {
        &mut db.item_scopes[self.0]
    }
}

/// 別モジュールを参照するUseアイテムアイテムを一意に特定するためのIDです。
/// 元データは`Database`に格納されています。
/// `use math::max;`
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UseItemId(Idx<UseItem>);
impl UseItemId {
    /// DBから使用宣言を取得します。
    pub fn lookup(self, db: &Database) -> &UseItem {
        &db.use_items[self.0]
    }
}

/// HIRのさまざまな値を保持するデータベースです。
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Database {
    item_scopes: Arena<ItemScope>,
    use_items: Arena<UseItem>,
    syntax_node_ptrs: Arena<SyntaxNodePtr>,
    idx_by_syntax_node_ptr: HashMap<SyntaxNodePtr, Idx<SyntaxNodePtr>>,
}
impl Database {
    /// データベースを作成します。
    pub fn new() -> Self {
        Self {
            item_scopes: Arena::default(),
            use_items: Arena::default(),
            syntax_node_ptrs: Arena::default(),
            idx_by_syntax_node_ptr: HashMap::default(),
        }
    }

    /// データベースに格納されているアイテムスコープ一覧を返します。
    pub fn item_scopes(&self) -> impl Iterator<Item = (ItemScopeId, &ItemScope)> {
        self.item_scopes
            .iter()
            .map(|(idx, item_scope)| (ItemScopeId(idx), item_scope))
    }

    /// データベースに格納されているUseアイテム一覧を返します。
    pub fn use_items(&self) -> impl Iterator<Item = (UseItemId, &UseItem)> {
        self.use_items
            .iter()
            .map(|(idx, use_item)| (UseItemId(idx), use_item))
    }

    /// データベースにUseアイテムを保存します。
    /// 保存時にデータを取得するためのIDを生成し返します。
    pub(crate) fn alloc_use_item(&mut self, use_item: UseItem) -> UseItemId {
        UseItemId(self.use_items.alloc(use_item))
    }

    /// データベースにアイテムスコープを保存します。
    /// 保存時にデータを取得するためのIDを生成し返します。
    pub(crate) fn alloc_item_scope(&mut self, item_scope: ItemScope) -> ItemScopeId {
        ItemScopeId(self.item_scopes.alloc(item_scope))
    }

    /// データベースに構文ノードを保存します。
    /// 保存時にデータを取得するためのIDを生成し返します。
    pub(crate) fn alloc_node<T: ast::AstNode>(&mut self, ast: &T, file: NailFile) -> AstId<T> {
        let ptr = SyntaxNodePtr::new(ast.syntax());
        let idx = self.syntax_node_ptrs.alloc(ptr.clone());
        let ast_ptr = AstPtr {
            raw: idx,
            _ty: std::marker::PhantomData,
        };

        self.idx_by_syntax_node_ptr.insert(ptr, idx);

        AstId(InFile {
            file,
            value: ast_ptr,
        })
    }

    /// 構文ノードのIDを取得します。
    /// 構文ノードがデータベースに存在しない場合はNoneを返します。
    pub(crate) fn lookup_ast_id<T: ast::AstNode>(
        &self,
        ast: &T,
        file: NailFile,
    ) -> Option<AstId<T>> {
        let ptr = SyntaxNodePtr::new(ast.syntax());
        let idx = self.idx_by_syntax_node_ptr.get(&ptr)?;

        let ast_ptr = AstPtr {
            raw: *idx,
            _ty: std::marker::PhantomData,
        };

        Some(AstId(InFile {
            file,
            value: ast_ptr,
        }))
    }
}
