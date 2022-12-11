mod item;
mod item_scope;

use std::collections::HashMap;

pub use item::{Function, FunctionIdx, Param, ParamIdx, Type};
pub use item_scope::{ItemScope, ItemScopeIdx, Parent};

use crate::{db::Database, string_interner::Interner, AstId, Name};

type BlockAstId = AstId<ast::Block>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemTree {
    pub top_level_scope: ItemScopeIdx,
    scope: HashMap<BlockAstId, ItemScopeIdx>,
    back_scope: HashMap<ItemScopeIdx, BlockAstId>,
    block_to_function: HashMap<BlockAstId, FunctionIdx>,
    function_to_block: HashMap<FunctionIdx, BlockAstId>,
}
impl ItemTree {
    pub fn top_level_scope<'a>(&self, db: &'a Database) -> &'a ItemScope {
        &db.item_scopes[self.top_level_scope]
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
        let mut top_level_scope = ItemScope::new(None);

        for stmt in ast.stmts() {
            self.build_stmt(stmt, &mut top_level_scope, Parent::TopLevel, db);
        }

        let top_level_scope = db.item_scopes.alloc(top_level_scope);
        ItemTree {
            top_level_scope,
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
                        db.params.alloc(param)
                    })
                    .collect::<Vec<_>>();
                let param_by_name = params
                    .iter()
                    .enumerate()
                    .filter_map(|param| {
                        let p = &db.params[*param.1];
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
                let function = db.functions.alloc(function);
                if let Some(name) = name {
                    current_scope.insert(name, function);
                }

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
