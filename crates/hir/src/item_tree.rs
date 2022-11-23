use std::collections::HashMap;

use la_arena::{Arena, Idx};

use crate::{AstId, ExprIdx, FileId, Name};

type BlockAstId = AstId<ast::Block>;

#[derive(Debug)]
pub struct Database {
    modules: Arena<Module>,
    functions: Arena<Function>,
    item_scopes: Arena<ItemScope>,
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
    pub body: ExprIdx,
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

    pub fn build_stmt(&mut self, stmt: ast::Stmt, item_scope: &mut ItemScope, db: &mut Database) {
        match stmt {
            ast::Stmt::VariableDef(_) | ast::Stmt::Expr(_) => (),
            ast::Stmt::FunctionDef(def) => {
                if let Some(body) = def.body() {
                    let scope = ItemScope::new();

                    todo!()
                } else {
                    return;
                }
            }
        }
    }
}
