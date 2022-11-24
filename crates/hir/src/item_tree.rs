use std::{collections::HashMap, hash::Hash};

use la_arena::{Arena, Idx};
use syntax::SyntaxNodePtr;

use crate::{string_interner::Interner, AstId, AstPtr, FileId, InFile, Name};

type BlockAstId = AstId<ast::Block>;

#[derive(Debug)]
pub struct Database {
    modules: Arena<Module>,
    pub functions: Arena<Function>,
    pub item_scopes: Arena<ItemScope>,
    syntax_node_ptrs: Arena<SyntaxNodePtr>,
    syntax_node_ptr_to_idx: HashMap<SyntaxNodePtr, Idx<SyntaxNodePtr>>,
    pub interner: Interner,
}
impl Database {
    pub fn new() -> Self {
        Self {
            modules: Arena::default(),
            functions: Arena::default(),
            item_scopes: Arena::default(),
            syntax_node_ptrs: Arena::default(),
            syntax_node_ptr_to_idx: HashMap::default(),
            interner: Interner::default(),
        }
    }

    fn alloc_node<T: ast::AstNode>(&mut self, ast: &T) -> AstId<T> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NailFile {
    pub file_id: FileId,
    pub module: ModuleIdx,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    origin: ModuleOrigin,
    scope: ItemScope,
    parent: Option<ModuleIdx>,
}

type ModuleIdx = Idx<Module>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleOrigin {
    File { definition: FileId },
    Block { definition: BlockAstId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Name,
    pub params: Vec<Param>,
    pub ast: AstId<ast::FunctionDef>,
}

pub type FunctionIdx = Idx<Function>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Parent {
    Root,
    Block(BlockAstId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemScope {
    functions: HashMap<Name, FunctionIdx>,
    parent: Option<Parent>,
}
impl ItemScope {
    pub fn new(parent: Option<Parent>) -> Self {
        Self {
            functions: HashMap::new(),
            parent,
        }
    }

    pub fn insert(&mut self, name: Name, function: FunctionIdx) {
        self.functions.insert(name, function);
    }

    pub fn lookup(&self, name: &Name, db: &Database, item_tree: &ItemTree) -> Option<FunctionIdx> {
        let function_idx = self.functions.get(name).copied();
        if function_idx.is_some() {
            return function_idx;
        }

        if let Some(parent) = &self.parent {
            match parent {
                Parent::Root => {
                    let root_item_scope = &db.item_scopes[item_tree.root_scope];
                    root_item_scope.lookup(name, db, item_tree)
                }
                Parent::Block(block) => {
                    let block_item_scope = &db.item_scopes[item_tree.scope[block]];
                    block_item_scope.lookup(name, db, item_tree)
                }
            }
        } else {
            None
        }
    }

    pub fn functions(&self) -> Vec<FunctionIdx> {
        self.functions.values().copied().collect()
    }
}

pub type ItemScopeIdx = Idx<ItemScope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    root_scope: ItemScopeIdx,
    scope: HashMap<BlockAstId, ItemScopeIdx>,
    back_scope: HashMap<ItemScopeIdx, BlockAstId>,
    block_to_function: HashMap<BlockAstId, FunctionIdx>,
    function_to_block: HashMap<FunctionIdx, BlockAstId>,
}
impl ItemTree {
    pub fn root_scope(&self) -> ItemScopeIdx {
        self.root_scope
    }

    pub fn block_scope(&self, ast: &BlockAstId) -> Option<ItemScopeIdx> {
        self.scope.get(ast).copied()
    }

    pub fn block_to_function(&self, block_id: &BlockAstId) -> Option<FunctionIdx> {
        self.block_to_function.get(block_id).copied()
    }

    pub fn function_to_block(&self, function_idx: &FunctionIdx) -> Option<BlockAstId> {
        Some(self.function_to_block.get(function_idx)?.clone())
    }
}

pub struct ItemTreeBuilderContext {
    pub scope: HashMap<BlockAstId, ItemScopeIdx>,
    pub back_scope: HashMap<ItemScopeIdx, BlockAstId>,
    pub block_to_function: HashMap<BlockAstId, FunctionIdx>,
    pub function_to_block: HashMap<FunctionIdx, BlockAstId>,
}
impl ItemTreeBuilderContext {
    pub fn new() -> Self {
        Self {
            scope: HashMap::new(),
            back_scope: HashMap::new(),
            block_to_function: HashMap::new(),
            function_to_block: HashMap::new(),
        }
    }

    pub fn build(mut self, ast: &ast::SourceFile, db: &mut Database) -> ItemTree {
        let mut root_scope = ItemScope::new(None);

        for stmt in ast.stmts() {
            self.build_stmt(stmt, &mut root_scope, Parent::Root, db);
        }

        let root_scope = db.item_scopes.alloc(root_scope);
        ItemTree {
            root_scope,
            scope: self.scope,
            back_scope: self.back_scope,
            block_to_function: self.block_to_function,
            function_to_block: self.function_to_block,
        }
    }

    pub fn build_stmt(
        &mut self,
        stmt: ast::Stmt,
        current_scope: &mut ItemScope,
        parent: Parent,
        db: &mut Database,
    ) -> Option<()> {
        match stmt {
            ast::Stmt::FunctionDef(def) => {
                let block = def.body()?;
                let name = Name::from_key(db.interner.intern(def.name()?.name()));
                let params = def
                    .params()?
                    .params()
                    .map(|param| {
                        let name = Name::from_key(db.interner.intern(param.name().unwrap().name()));
                        Param { name }
                    })
                    .collect();

                let ast_id = db.alloc_node(&def);
                let function = Function {
                    name,
                    params,
                    ast: ast_id,
                };
                let function = db.functions.alloc(function);
                current_scope.insert(name, function);

                let block = self.build_block(block, parent, db);
                self.block_to_function.insert(block.clone(), function);
                self.function_to_block.insert(function, block);
            }
            ast::Stmt::Expr(expr) => match expr {
                ast::Expr::Block(block) => {
                    self.build_block(block, parent, db);
                }
                _ => {}
            },
            ast::Stmt::VariableDef(_) => (),
        }

        Some(())
    }

    pub fn build_block(
        &mut self,
        block: ast::Block,
        parent: Parent,
        db: &mut Database,
    ) -> AstId<ast::Block> {
        let mut scope = ItemScope::new(Some(parent.clone()));
        for stmt in block.stmts() {
            self.build_stmt(stmt, &mut scope, parent.clone(), db);
        }

        let block = db.alloc_node(&block);
        let scope = db.item_scopes.alloc(scope);
        self.scope.insert(block.clone(), scope);
        self.back_scope.insert(scope, block.clone());

        block
    }
}
