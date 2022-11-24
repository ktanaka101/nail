mod body;
mod item_tree;
pub mod string_interner;

use std::marker::PhantomData;

use body::RootBodyLowerContext;
use item_tree::{Database, ItemTree, ItemTreeBuilderContext};
use la_arena::Idx;

use ast::Ast;
pub use body::BodyLowerContext;
use string_interner::Key;
use syntax::SyntaxNodePtr;

pub fn lower(
    ast: ast::SourceFile,
) -> (
    RootBodyLowerContext,
    BodyLowerContext,
    Vec<Stmt>,
    Database,
    ItemTree,
) {
    let mut db = Database::new();
    let item_tree_builder = ItemTreeBuilderContext::new();
    let item_tree = item_tree_builder.build(&ast, &mut db);

    let mut root_ctx = RootBodyLowerContext::new();

    let mut ctx = BodyLowerContext::new();
    let stmts = ast
        .stmts()
        .filter_map(|stmt| ctx.lower_stmt(stmt, &mut root_ctx, &db, &item_tree))
        .collect();
    (root_ctx, ctx, stmts, db, item_tree)
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
    VariableDef {
        name: Name,
        value: ExprIdx,
    },
    Expr(ExprIdx),
    FunctionDef {
        name: Name,
        params: Vec<Name>,
        body: ExprIdx,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(i64),
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Binary {
        op: BinaryOp,
        lhs: ExprIdx,
        rhs: ExprIdx,
    },
    Literal(Literal),
    Unary {
        op: UnaryOp,
        expr: ExprIdx,
    },
    VariableRef {
        var: Symbol,
        name: Name,
    },
    Block(Block),
    Missing,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Symbol {
    Param,
    Local(ExprIdx),
    Function,
    Missing,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    stmts: Vec<Stmt>,
    tail: Option<ExprIdx>,
    ast: AstId<ast::Block>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
}
