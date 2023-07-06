mod body;
mod db;
mod input;
mod item_tree;
pub mod string_interner;

use std::{collections::HashMap, marker::PhantomData};

use ast::{Ast, AstNode};
pub use body::{BodyLower, ExprId, FunctionBodyId, SharedBodyLowerContext};
pub use db::{Database, FunctionId, ItemScopeId, ModuleId, ParamId};
pub use input::{
    FileId, FilelessSourceDatabase, FixtureDatabase, SourceDatabase, SourceDatabaseTrait,
};
use item_tree::ItemTreeBuilderContext;
pub use item_tree::{Function, ItemDefId, ItemTree, Param, Type};
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
    pub top_level_items: Vec<ItemDefId>,
    pub entry_point: Option<FunctionId>,
    pub db: Database,
    pub item_tree: ItemTree,
    pub interner: Interner,
    pub errors: Vec<LowerError>,
}
impl LowerResult {
    pub fn function_body_by_function(&self, function: FunctionId) -> Option<&Expr> {
        let body_block = self.item_tree.block_id_by_function(&function)?;
        self.shared_ctx.function_body_by_block(body_block)
    }
}

pub fn parse_root(path: &str, source_db: &mut dyn SourceDatabaseTrait) -> LowerResult {
    let file_id = source_db.register_file_with_read(path);

    let ast = parser::parse(source_db.content(file_id).unwrap());
    let ast = ast::SourceFile::cast(ast.syntax()).unwrap();

    lower(file_id, ast)
}

pub fn lower(file_id: FileId, ast: ast::SourceFile) -> LowerResult {
    let mut interner = Interner::new();
    let mut db = Database::new();
    let item_tree_builder = ItemTreeBuilderContext::new(file_id, &mut interner);
    let item_tree = item_tree_builder.build(file_id, &ast, &mut db);

    let mut shared_ctx = SharedBodyLowerContext::new();

    let mut top_level_ctx = BodyLower::new(file_id, HashMap::new());
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
        top_level_items,
        entry_point,
        db,
        item_tree,
        interner,
        errors,
    }
}

fn get_entry_point(
    top_level_items: &[ItemDefId],
    db: &Database,
    interner: &Interner,
) -> Option<FunctionId> {
    for item in top_level_items {
        match item {
            ItemDefId::Function(function_id) => {
                let function = function_id.lookup(db);
                if let Some(name) = function.name {
                    if interner.lookup(name.key()) == "main" {
                        return Some(*function_id);
                    }
                }
            }
            ItemDefId::Module(_) => (),
            ItemDefId::UseItem(_) => (),
        }
    }

    None
}

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
    VariableDef { name: Name, value: ExprId },
    ExprStmt { expr: ExprId, has_semicolon: bool },
    Item { item: ItemDefId },
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
    Symbol(Symbol),
    Binary {
        op: ast::BinaryOp,
        lhs: ExprId,
        rhs: ExprId,
    },
    Literal(Literal),
    Unary {
        op: ast::UnaryOp,
        expr: ExprId,
    },
    Call {
        callee: Symbol,
        args: Vec<ExprId>,
    },
    Block(Block),
    If {
        condition: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    },
    Return {
        value: Option<ExprId>,
    },
    Missing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    segments: Vec<Name>,
}
impl Path {
    pub fn segments(&self) -> &[Name] {
        self.segments.as_slice()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Symbol {
    Param { name: Name, param: ParamId },
    Local { name: Name, expr: ExprId },
    Function { path: Path, function: FunctionId },
    Missing { path: Path },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub tail: Option<ExprId>,
    pub ast: AstId<ast::BlockExpr>,
}
impl Block {
    pub fn tail<'a>(&self, ctx: &'a SharedBodyLowerContext) -> Option<&'a Expr> {
        if let Some(tail) = self.tail {
            Some(tail.lookup(ctx))
        } else {
            None
        }
    }
}
