mod body;
mod db;
mod item_tree;
pub mod string_interner;

use std::{collections::HashMap, marker::PhantomData};

use ast::Ast;
pub use body::{BodyLower, SharedBodyLowerContext};
pub use db::Database;
use item_tree::ItemTreeBuilderContext;
pub use item_tree::{Function, FunctionIdx, Item, ItemTree, Param, ParamIdx, Type};
use la_arena::Idx;
use string_interner::{Interner, Key};
use syntax::SyntaxNodePtr;

#[derive(Debug)]
pub enum LowerError {
    UndefinedEntryPoint,
}

#[derive(Debug)]
pub struct LowerResult {
    pub shared_ctx: SharedBodyLowerContext,
    pub top_level_ctx: BodyLower,
    pub top_level_items: Vec<Item>,
    pub entry_point: Option<FunctionIdx>,
    pub db: Database,
    pub item_tree: ItemTree,
    pub interner: Interner,
    pub errors: Vec<LowerError>,
}
impl LowerResult {
    pub fn function_body_by_function(&self, function: &FunctionIdx) -> Option<&Expr> {
        let body_block = &self.item_tree.block_idx_by_function(function)?;
        self.shared_ctx.function_body_by_block(body_block)
    }
}

pub fn lower(ast: ast::SourceFile) -> LowerResult {
    let mut interner = Interner::new();
    let mut db = Database::new();
    let item_tree_builder = ItemTreeBuilderContext::new(&mut interner);
    let item_tree = item_tree_builder.build(&ast, &mut db);

    let mut shared_ctx = SharedBodyLowerContext::new();

    let mut top_level_ctx = BodyLower::new(HashMap::new());
    let top_level_items = ast
        .items()
        .filter_map(|item| {
            top_level_ctx.lower_toplevel(item, &mut shared_ctx, &db, &item_tree, &mut interner)
        })
        .collect::<Vec<_>>();

    let mut errors = vec![];
    let entry_point = get_entry_point(&top_level_items, &db, &interner);
    if entry_point.is_none() {
        errors.push(LowerError::UndefinedEntryPoint);
    }

    LowerResult {
        shared_ctx,
        top_level_ctx,
        top_level_items,
        entry_point,
        db,
        item_tree,
        interner,
        errors,
    }
}

fn get_entry_point(
    top_level_items: &[Item],
    db: &Database,
    interner: &Interner,
) -> Option<FunctionIdx> {
    for item in top_level_items {
        match item {
            Item::Function(function_idx) => {
                let function = &db.functions[*function_idx];
                if let Some(name) = function.name {
                    if interner.lookup(name.key()) == "main" {
                        return Some(*function_idx);
                    }
                }
            }
        }
    }

    None
}

pub type ExprIdx = Idx<Expr>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstId<T: Ast>(InFile<AstPtr<T>>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstPtr<T: Ast> {
    raw: AstIdx,
    _ty: PhantomData<fn() -> T>,
}

pub type AstIdx = Idx<SyntaxNodePtr>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InFile<T> {
    pub file_id: FileId,
    pub value: T,
}

/// todo: resolve file paths
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(Key);
impl Name {
    pub fn key(&self) -> Key {
        self.0
    }

    fn from_key(key: Key) -> Self {
        Self(key)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    VariableDef { name: Name, value: ExprIdx },
    ExprStmt { expr: ExprIdx, has_semicolon: bool },
    Item { item: Item },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(u64),
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Binary {
        op: ast::BinaryOp,
        lhs: ExprIdx,
        rhs: ExprIdx,
    },
    Literal(Literal),
    Unary {
        op: ast::UnaryOp,
        expr: ExprIdx,
    },
    VariableRef {
        var: Symbol,
    },
    Call {
        callee: Symbol,
        args: Vec<ExprIdx>,
    },
    Block(Block),
    If {
        condition: ExprIdx,
        then_branch: ExprIdx,
        else_branch: Option<ExprIdx>,
    },
    Return {
        value: Option<ExprIdx>,
    },
    Missing,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Symbol {
    Param { name: Name, param: ParamIdx },
    Local { name: Name, expr: ExprIdx },
    Function { name: Name, function: FunctionIdx },
    Missing { name: Name },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub tail: Option<ExprIdx>,
    pub ast: AstId<ast::Block>,
}
impl Block {
    pub fn tail<'a>(&self, ctx: &'a SharedBodyLowerContext) -> Option<&'a Expr> {
        if let Some(tail) = self.tail {
            Some(&ctx.exprs[tail])
        } else {
            None
        }
    }
}
