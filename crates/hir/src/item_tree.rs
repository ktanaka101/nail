use std::collections::HashMap;

use ast::AstNode;
use la_arena::{Arena, Idx};
use syntax::SyntaxNodePtr;

use crate::{string_interner::Interner, AstId, AstPtr, FileId, InFile, Name};

type BlockAstId = AstId<ast::Block>;

#[derive(Debug)]
pub struct Database {
    modules: Arena<Module>,
    functions: Arena<Function>,
    item_scopes: Arena<ItemScope>,
    syntax_node_ptrs: Arena<SyntaxNodePtr>,
    interner: Interner,
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
    name: Name,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Name,
    pub params: Vec<Param>,
    pub ast: AstId<ast::FunctionDef>,
}

pub type FunctionIdx = Idx<Function>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemScope {
    functions: HashMap<Name, FunctionIdx>,
}
impl ItemScope {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }
}

pub type ItemScopeIdx = Idx<ItemScope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    pub root_scope: ItemScopeIdx,
    pub scope: HashMap<BlockAstId, ItemScopeIdx>,
}

struct ItemTreeBuilderContext {
    pub scope: HashMap<BlockAstId, ItemScopeIdx>,
}
impl ItemTreeBuilderContext {
    pub fn new() -> Self {
        Self {
            scope: HashMap::new(),
        }
    }

    pub fn build(mut self, ast: ast::SourceFile, db: &mut Database) -> ItemTree {
        let mut root_scope = ItemScope::new();
        for stmt in ast.stmts() {
            self.build_stmt(stmt, &mut root_scope, db);
        }

        let root_scope = db.item_scopes.alloc(root_scope);
        ItemTree {
            root_scope,
            scope: self.scope,
        }
    }

    pub fn build_stmt(
        &mut self,
        stmt: ast::Stmt,
        current_scope: &mut ItemScope,
        db: &mut Database,
    ) -> Option<()> {
        match stmt {
            ast::Stmt::VariableDef(_) | ast::Stmt::Expr(_) => (),
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

                let ptr = SyntaxNodePtr::new(def.syntax());
                let idx = db.syntax_node_ptrs.alloc(ptr);
                let ast_ptr = AstPtr {
                    raw: idx,
                    _ty: std::marker::PhantomData,
                };
                let ptr = AstId::<ast::FunctionDef>(InFile {
                    file_id: FileId,
                    value: ast_ptr,
                });

                let function = Function {
                    name,
                    params,
                    ast: ptr,
                };
                let function = db.functions.alloc(function);
                current_scope.functions.insert(name, function);

                self.build_block(block, db);

                todo!()
            }
        }

        Some(())
    }

    pub fn build_block(&mut self, block: ast::Block, db: &mut Database) {
        todo!()
    }
}
