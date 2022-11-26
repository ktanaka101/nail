use std::collections::HashMap;

use la_arena::Idx;

use crate::db::Database;
use crate::{string_interner::Interner, AstId, Name};

type BlockAstId = AstId<ast::Block>;

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

    pub fn lookup(
        &self,
        name_str: &str,
        db: &Database,
        item_tree: &ItemTree,
        interner: &Interner,
    ) -> Option<FunctionIdx> {
        let function_name_key = interner.get_key(name_str);
        if let Some(function_name_key) = function_name_key {
            let name = &Name::from_key(function_name_key);
            let function_idx = self.functions.get(name).copied();
            if function_idx.is_some() {
                return function_idx;
            }
        }

        if let Some(parent) = &self.parent {
            match parent {
                Parent::Root => {
                    let root_item_scope = &db.item_scopes[item_tree.root_scope];
                    root_item_scope.lookup(name_str, db, item_tree, interner)
                }
                Parent::Block(block) => {
                    let block_item_scope = &db.item_scopes[item_tree.scope[block]];
                    block_item_scope.lookup(name_str, db, item_tree, interner)
                }
            }
        } else {
            None
        }
    }

    pub fn functions(&self) -> Vec<FunctionIdx> {
        let mut functions = self.functions.values().copied().collect::<Vec<_>>();
        functions.sort_by_cached_key(|idx| idx.into_raw());
        functions
    }
}

pub type ItemScopeIdx = Idx<ItemScope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    pub root_scope: ItemScopeIdx,
    scope: HashMap<BlockAstId, ItemScopeIdx>,
    back_scope: HashMap<ItemScopeIdx, BlockAstId>,
    block_to_function: HashMap<BlockAstId, FunctionIdx>,
    function_to_block: HashMap<FunctionIdx, BlockAstId>,
}
impl ItemTree {
    pub fn root_scope<'a>(&self, db: &'a Database) -> &'a ItemScope {
        &db.item_scopes[self.root_scope]
    }

    pub fn block_scope_idx(&self, ast: &BlockAstId) -> Option<ItemScopeIdx> {
        self.scope.get(ast).copied()
    }

    pub fn block_scope<'a>(&self, db: &'a Database, ast: &BlockAstId) -> Option<&'a ItemScope> {
        self.block_scope_idx(ast).map(|idx| &db.item_scopes[idx])
    }

    pub fn block_to_function_idx(&self, block_ast_id: &BlockAstId) -> Option<FunctionIdx> {
        self.block_to_function.get(block_ast_id).copied()
    }

    pub fn block_to_function<'a>(
        &self,
        db: &'a Database,
        block_ast_id: &BlockAstId,
    ) -> Option<&'a Function> {
        self.block_to_function_idx(block_ast_id)
            .map(|idx| &db.functions[idx])
    }

    pub fn function_to_block(&self, function_idx: &FunctionIdx) -> Option<BlockAstId> {
        Some(self.function_to_block.get(function_idx)?.clone())
    }
}

pub struct ItemTreeBuilderContext<'a> {
    pub scope: HashMap<BlockAstId, ItemScopeIdx>,
    pub back_scope: HashMap<ItemScopeIdx, BlockAstId>,
    pub block_to_function: HashMap<BlockAstId, FunctionIdx>,
    pub function_to_block: HashMap<FunctionIdx, BlockAstId>,
    pub interner: &'a mut Interner,
}
impl<'a> ItemTreeBuilderContext<'a> {
    pub fn new(interner: &'a mut Interner) -> Self {
        Self {
            scope: HashMap::new(),
            back_scope: HashMap::new(),
            block_to_function: HashMap::new(),
            function_to_block: HashMap::new(),
            interner,
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
                let name = Name::from_key(self.interner.intern(def.name()?.name()));
                let params = def
                    .params()?
                    .params()
                    .map(|param| {
                        let name =
                            Name::from_key(self.interner.intern(param.name().unwrap().name()));
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
            ast::Stmt::Expr(expr) => self.build_expr(expr, current_scope, parent, db)?,
            ast::Stmt::VariableDef(def) => {
                self.build_expr(def.value()?, current_scope, parent, db)?
            }
        }

        Some(())
    }

    pub fn build_expr(
        &mut self,
        expr: ast::Expr,
        _current_scope: &mut ItemScope,
        parent: Parent,
        db: &mut Database,
    ) -> Option<()> {
        match expr {
            ast::Expr::BinaryExpr(binary) => {
                self.build_expr(binary.lhs()?, _current_scope, parent.clone(), db)?;
                self.build_expr(binary.rhs()?, _current_scope, parent, db)?;
            }
            ast::Expr::ParenExpr(paren) => {
                self.build_expr(paren.expr()?, _current_scope, parent, db)?;
            }
            ast::Expr::UnaryExpr(unary) => {
                self.build_expr(unary.expr()?, _current_scope, parent, db)?;
            }
            ast::Expr::Block(block) => {
                self.build_block(block, parent, db);
            }
            _ => (),
        };

        Some(())
    }

    pub fn build_block(
        &mut self,
        block: ast::Block,
        parent: Parent,
        db: &mut Database,
    ) -> AstId<ast::Block> {
        let mut scope = ItemScope::new(Some(parent));
        let block_ast_id = db.alloc_node(&block);
        let current = Parent::Block(block_ast_id.clone());
        for stmt in block.stmts() {
            self.build_stmt(stmt, &mut scope, current.clone(), db);
        }

        let scope = db.item_scopes.alloc(scope);
        self.scope.insert(block_ast_id.clone(), scope);
        self.back_scope.insert(scope, block_ast_id.clone());

        block_ast_id
    }
}
