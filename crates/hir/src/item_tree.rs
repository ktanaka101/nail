mod item;
mod item_scope;

use std::collections::HashMap;

pub use item::{Function, ItemDefId, Module, Param, Type};
pub use item_scope::{ItemScope, Parent};

use crate::{
    db::{Database, FunctionId, ItemScopeId, ModuleId},
    string_interner::Interner,
    AstId, Name,
};

type BlockAstId = AstId<ast::Block>;
type ModuleAstId = AstId<ast::Module>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    pub top_level_scope: ItemScopeId,
    scope_by_block: HashMap<BlockAstId, ItemScopeId>,
    block_by_scope: HashMap<ItemScopeId, BlockAstId>,
    function_by_block: HashMap<BlockAstId, FunctionId>,
    block_by_function: HashMap<FunctionId, BlockAstId>,

    scope_by_module: HashMap<ModuleAstId, ItemScopeId>,
    module_by_ast_module: HashMap<ModuleAstId, ModuleId>,
}
impl ItemTree {
    pub fn top_level_scope<'a>(&self, db: &'a Database) -> &'a ItemScope {
        self.top_level_scope.lookup(db)
    }

    pub fn scope_id_by_block(&self, ast: &BlockAstId) -> Option<ItemScopeId> {
        self.scope_by_block.get(ast).copied()
    }

    pub fn scope_by_block<'a>(&self, db: &'a Database, ast: &BlockAstId) -> Option<&'a ItemScope> {
        self.scope_id_by_block(ast)
            .map(|scope_id| scope_id.lookup(db))
    }

    pub fn scope_by_module<'a>(
        &self,
        db: &'a Database,
        ast_id: &ModuleAstId,
    ) -> Option<&'a ItemScope> {
        let scope_id = self.scope_by_module.get(ast_id);
        scope_id.map(|scope_id| scope_id.lookup(db))
    }

    pub fn function_id_by_block(&self, block_ast_id: &BlockAstId) -> Option<FunctionId> {
        self.function_by_block.get(block_ast_id).copied()
    }

    pub fn function_by_block<'a>(
        &self,
        db: &'a Database,
        block_ast_id: &BlockAstId,
    ) -> Option<&'a Function> {
        self.function_id_by_block(block_ast_id)
            .map(|fn_id| fn_id.lookup(db))
    }

    pub fn block_id_by_function(&self, function_id: &FunctionId) -> Option<BlockAstId> {
        Some(self.block_by_function.get(function_id)?.clone())
    }

    pub fn module_id_by_ast_module(&self, module_ast_id: ModuleAstId) -> Option<ModuleId> {
        self.module_by_ast_module.get(&module_ast_id).copied()
    }

    pub fn module_by_ast_module_id<'a>(
        &self,
        db: &'a Database,
        module_ast_id: ModuleAstId,
    ) -> Option<&'a Module> {
        self.module_id_by_ast_module(module_ast_id)
            .map(|module_id| module_id.lookup(db))
    }
}

pub(crate) struct ItemTreeBuilderContext<'a> {
    scope_by_block: HashMap<BlockAstId, ItemScopeId>,
    block_by_scope: HashMap<ItemScopeId, BlockAstId>,
    function_by_block: HashMap<BlockAstId, FunctionId>,
    block_by_function: HashMap<FunctionId, BlockAstId>,

    scope_by_module: HashMap<ModuleAstId, ItemScopeId>,
    module_by_ast_module: HashMap<ModuleAstId, ModuleId>,

    interner: &'a mut Interner,
}
impl<'a> ItemTreeBuilderContext<'a> {
    pub(crate) fn new(interner: &'a mut Interner) -> Self {
        Self {
            scope_by_block: HashMap::new(),
            block_by_scope: HashMap::new(),
            function_by_block: HashMap::new(),
            block_by_function: HashMap::new(),
            scope_by_module: HashMap::new(),
            module_by_ast_module: HashMap::new(),
            interner,
        }
    }

    pub(crate) fn build(mut self, ast: &ast::SourceFile, db: &mut Database) -> ItemTree {
        let top_level_scope = ItemScope::new(None);
        let top_level_scope_id = db.alloc_item_scope(top_level_scope);

        for item in ast.items() {
            self.build_item(item, top_level_scope_id, Parent::TopLevel, db);
        }

        ItemTree {
            top_level_scope: top_level_scope_id,
            scope_by_block: self.scope_by_block,
            block_by_scope: self.block_by_scope,
            function_by_block: self.function_by_block,
            block_by_function: self.block_by_function,
            scope_by_module: self.scope_by_module,
            module_by_ast_module: self.module_by_ast_module,
        }
    }

    fn build_item(
        &mut self,
        item: ast::Item,
        current_scope: ItemScopeId,
        parent: Parent,
        db: &mut Database,
    ) -> Option<ItemDefId> {
        match item {
            ast::Item::FunctionDef(def) => {
                let block = def.body()?;
                let params = def
                    .params()?
                    .params()
                    .enumerate()
                    .map(|(pos, param)| {
                        let name = if let Some(name) = param.name() {
                            Some(Name::from_key(self.interner.intern(name.name())))
                        } else {
                            None
                        };
                        let ty = self.lower_ty(param.ty());
                        let param = Param { name, ty, pos };
                        db.alloc_param(param)
                    })
                    .collect::<Vec<_>>();
                let param_by_name = params
                    .iter()
                    .enumerate()
                    .filter_map(|param| {
                        let p = param.1.lookup(db);
                        p.name.map(|name| (name, *param.1))
                    })
                    .collect::<HashMap<_, _>>();
                let return_type = if let Some(return_type) = def.return_type() {
                    self.lower_ty(return_type.ty())
                } else {
                    Type::Unit
                };

                let name = if let Some(name) = def.name() {
                    Some(Name::from_key(self.interner.intern(name.name())))
                } else {
                    None
                };

                let ast_id = db.alloc_node(&def);
                let function = Function {
                    name,
                    params,
                    param_by_name,
                    return_type,
                    ast: ast_id,
                };
                let function = db.alloc_function(function);
                if let Some(name) = name {
                    current_scope.lookup_mut(db).insert_function(name, function);
                }

                let block = self.build_block(block, parent, db);
                self.function_by_block.insert(block.clone(), function);
                self.block_by_function.insert(function, block);

                Some(ItemDefId::Function(function))
            }
            ast::Item::Module(module) => {
                let module_id = self.build_module(&module, current_scope, parent, db)?;
                Some(ItemDefId::Module(module_id))
            }
        }
    }

    fn build_stmt(
        &mut self,
        stmt: ast::Stmt,
        current_scope: ItemScopeId,
        parent: Parent,
        db: &mut Database,
    ) -> Option<()> {
        match stmt {
            ast::Stmt::ExprStmt(expr_stmt) => {
                self.build_expr(expr_stmt.expr()?, current_scope, parent, db)?
            }
            ast::Stmt::VariableDef(def) => {
                self.build_expr(def.value()?, current_scope, parent, db)?
            }
            ast::Stmt::Item(item) => {
                self.build_item(item, current_scope, parent, db)?;
            }
        }

        Some(())
    }

    fn lower_ty(&mut self, ty: Option<ast::Type>) -> Type {
        let ident = match ty.and_then(|ty| ty.ty()) {
            Some(ident) => ident,
            None => return Type::Unknown,
        };

        let type_name = ident.name();
        match type_name {
            "int" => Type::Integer,
            "string" => Type::String,
            "char" => Type::Char,
            "bool" => Type::Boolean,
            _ => Type::Unknown,
        }
    }

    fn build_expr(
        &mut self,
        expr: ast::Expr,
        _current_scope: ItemScopeId,
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
            ast::Expr::IfExpr(if_expr) => {
                self.build_expr(if_expr.condition()?, _current_scope, parent.clone(), db)?;
                self.build_block(if_expr.then_branch()?, parent.clone(), db);
                self.build_block(if_expr.else_branch()?, parent, db);
            }
            _ => (),
        };

        Some(())
    }

    fn build_block(
        &mut self,
        block: ast::Block,
        parent: Parent,
        db: &mut Database,
    ) -> AstId<ast::Block> {
        let scope = ItemScope::new(Some(parent));
        let scope_id = db.alloc_item_scope(scope);
        let block_ast_id = db.alloc_node(&block);
        let current = Parent::SubLevel(scope_id);
        for stmt in block.stmts() {
            self.build_stmt(stmt, scope_id, current.clone(), db);
        }

        self.scope_by_block.insert(block_ast_id.clone(), scope_id);
        self.block_by_scope.insert(scope_id, block_ast_id.clone());

        block_ast_id
    }

    fn build_module(
        &mut self,
        module: &ast::Module,
        current_scope: ItemScopeId,
        parent: Parent,
        db: &mut Database,
    ) -> Option<ModuleId> {
        if let Some(item_list) = module.items() {
            let scope = ItemScope::new(Some(parent));
            let scope_id = db.alloc_item_scope(scope);

            let current = Parent::SubLevel(scope_id);
            let mut items = vec![];
            for item in item_list.items() {
                if let Some(item) = self.build_item(item, scope_id, current.clone(), db) {
                    items.push(item);
                }
            }

            let module_ast_id = db.alloc_node(module);
            self.scope_by_module.insert(module_ast_id.clone(), scope_id);

            let module_name = Name::from_key(self.interner.intern(module.name().unwrap().name()));
            let hir_module = Module {
                name: module_name,
                items,
            };
            let module_id = db.alloc_module(hir_module);
            current_scope
                .lookup_mut(db)
                .insert_module(module_name, module_id);
            self.module_by_ast_module.insert(module_ast_id, module_id);

            Some(module_id)
        } else {
            None
        }
    }
}
