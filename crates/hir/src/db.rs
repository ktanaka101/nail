use std::collections::HashMap;

use la_arena::{Arena, Idx};
use syntax::SyntaxNodePtr;

use crate::{AstId, AstPtr, InFile, NailFile};

/// HIRのさまざまな値を保持するデータベースです。
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Database {
    syntax_node_ptrs: Arena<SyntaxNodePtr>,
    idx_by_syntax_node_ptr: HashMap<SyntaxNodePtr, Idx<SyntaxNodePtr>>,
}
impl Database {
    /// データベースを作成します。
    pub fn new() -> Self {
        Self {
            syntax_node_ptrs: Arena::default(),
            idx_by_syntax_node_ptr: HashMap::default(),
        }
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
