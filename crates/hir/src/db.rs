use std::collections::HashMap;

use la_arena::{Arena, Idx};
use syntax::SyntaxNodePtr;

use crate::{
    item_tree::{Function, ItemScope, Param},
    AstId, AstPtr, FileId, InFile,
};

#[derive(Debug, Default)]
pub struct Database {
    pub functions: Arena<Function>,
    pub params: Arena<Param>,
    pub item_scopes: Arena<ItemScope>,
    syntax_node_ptrs: Arena<SyntaxNodePtr>,
    idx_by_syntax_node_ptr: HashMap<SyntaxNodePtr, Idx<SyntaxNodePtr>>,
}
impl Database {
    pub fn new() -> Self {
        Self {
            functions: Arena::default(),
            params: Arena::default(),
            item_scopes: Arena::default(),
            syntax_node_ptrs: Arena::default(),
            idx_by_syntax_node_ptr: HashMap::default(),
        }
    }

    pub fn alloc_node<T: ast::AstNode>(&mut self, ast: &T) -> AstId<T> {
        let ptr = SyntaxNodePtr::new(ast.syntax());
        let idx = self.syntax_node_ptrs.alloc(ptr.clone());
        let ast_ptr = AstPtr {
            raw: idx,
            _ty: std::marker::PhantomData,
        };

        self.idx_by_syntax_node_ptr.insert(ptr, idx);

        AstId(InFile {
            file_id: FileId,
            value: ast_ptr,
        })
    }

    pub fn lookup_ast_id<T: ast::AstNode>(&self, ast: &T) -> Option<AstId<T>> {
        let ptr = SyntaxNodePtr::new(ast.syntax());
        let idx = self.idx_by_syntax_node_ptr.get(&ptr)?;

        let ast_ptr = AstPtr {
            raw: *idx,
            _ty: std::marker::PhantomData,
        };

        Some(AstId(InFile {
            file_id: FileId,
            value: ast_ptr,
        }))
    }
}
