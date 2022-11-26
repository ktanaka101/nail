use std::collections::HashMap;

use la_arena::{Arena, Idx};

use syntax::SyntaxNodePtr;

use crate::item_tree::{Function, ItemScope};
use crate::{AstId, AstPtr, FileId, InFile};

#[derive(Debug, Default)]
pub struct Database {
    pub functions: Arena<Function>,
    pub item_scopes: Arena<ItemScope>,
    syntax_node_ptrs: Arena<SyntaxNodePtr>,
    syntax_node_ptr_to_idx: HashMap<SyntaxNodePtr, Idx<SyntaxNodePtr>>,
}
impl Database {
    pub fn new() -> Self {
        Self {
            functions: Arena::default(),
            item_scopes: Arena::default(),
            syntax_node_ptrs: Arena::default(),
            syntax_node_ptr_to_idx: HashMap::default(),
        }
    }

    pub fn alloc_node<T: ast::AstNode>(&mut self, ast: &T) -> AstId<T> {
        let ptr = SyntaxNodePtr::new(ast.syntax());
        let idx = self.syntax_node_ptrs.alloc(ptr.clone());
        let ast_ptr = AstPtr {
            raw: idx,
            _ty: std::marker::PhantomData,
        };

        self.syntax_node_ptr_to_idx.insert(ptr, idx);

        AstId(InFile {
            file_id: FileId,
            value: ast_ptr,
        })
    }

    pub fn lookup_ast_id<T: ast::AstNode>(&self, ast: &T) -> Option<AstId<T>> {
        let ptr = SyntaxNodePtr::new(ast.syntax());
        let idx = self.syntax_node_ptr_to_idx.get(&ptr).unwrap();

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
