use std::collections::HashMap;

use la_arena::{Arena, Idx};
use syntax::SyntaxNodePtr;

use crate::{
    item_tree::{Function, ItemScope, Module, Param},
    AstId, AstPtr, FileId, InFile,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(Idx<Function>);
impl FunctionId {
    pub fn lookup(self, db: &Database) -> &Function {
        &db.functions[self.0]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ParamId(Idx<Param>);
impl ParamId {
    pub fn lookup(self, db: &Database) -> &Param {
        &db.params[self.0]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemScopeId(Idx<ItemScope>);
impl ItemScopeId {
    pub fn lookup(self, db: &Database) -> &ItemScope {
        &db.item_scopes[self.0]
    }

    pub(crate) fn lookup_mut(self, db: &mut Database) -> &mut ItemScope {
        &mut db.item_scopes[self.0]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(Idx<Module>);
impl ModuleId {
    pub fn lookup(self, db: &Database) -> &Module {
        &db.modules[self.0]
    }
}

#[derive(Debug, Default)]
pub struct Database {
    functions: Arena<Function>,
    params: Arena<Param>,
    item_scopes: Arena<ItemScope>,
    modules: Arena<Module>,
    syntax_node_ptrs: Arena<SyntaxNodePtr>,
    idx_by_syntax_node_ptr: HashMap<SyntaxNodePtr, Idx<SyntaxNodePtr>>,
}
impl Database {
    pub fn new() -> Self {
        Self {
            functions: Arena::default(),
            params: Arena::default(),
            item_scopes: Arena::default(),
            modules: Arena::default(),
            syntax_node_ptrs: Arena::default(),
            idx_by_syntax_node_ptr: HashMap::default(),
        }
    }

    pub fn functions(&self) -> impl Iterator<Item = (FunctionId, &Function)> {
        self.functions
            .iter()
            .map(|(idx, function)| (FunctionId(idx), function))
    }

    pub fn params(&self) -> impl Iterator<Item = (ParamId, &Param)> {
        self.params.iter().map(|(idx, param)| (ParamId(idx), param))
    }

    pub fn item_scopes(&self) -> impl Iterator<Item = (ItemScopeId, &ItemScope)> {
        self.item_scopes
            .iter()
            .map(|(idx, item_scope)| (ItemScopeId(idx), item_scope))
    }

    pub fn modules(&self) -> impl Iterator<Item = (ModuleId, &Module)> {
        self.modules
            .iter()
            .map(|(idx, module)| (ModuleId(idx), module))
    }

    pub(crate) fn alloc_param(&mut self, param: Param) -> ParamId {
        ParamId(self.params.alloc(param))
    }

    pub(crate) fn alloc_function(&mut self, function: Function) -> FunctionId {
        FunctionId(self.functions.alloc(function))
    }

    pub(crate) fn alloc_module(&mut self, module: Module) -> ModuleId {
        ModuleId(self.modules.alloc(module))
    }

    pub(crate) fn alloc_item_scope(&mut self, item_scope: ItemScope) -> ItemScopeId {
        ItemScopeId(self.item_scopes.alloc(item_scope))
    }

    pub(crate) fn alloc_node<T: ast::AstNode>(&mut self, ast: &T) -> AstId<T> {
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

    pub(crate) fn lookup_ast_id<T: ast::AstNode>(&self, ast: &T) -> Option<AstId<T>> {
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
