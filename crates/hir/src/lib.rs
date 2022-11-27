mod body;
mod db;
mod item_tree;
pub mod string_interner;

use std::marker::PhantomData;

use la_arena::Idx;

use body::RootBodyLowerContext;
use db::Database;
use item_tree::{FunctionIdx, ItemTree, ItemTreeBuilderContext};

use ast::Ast;
pub use body::BodyLowerContext;
use string_interner::{Interner, Key};
use syntax::SyntaxNodePtr;

pub fn lower(
    ast: ast::SourceFile,
) -> (
    RootBodyLowerContext,
    BodyLowerContext,
    Vec<Stmt>,
    Database,
    ItemTree,
    Interner,
) {
    let mut interner = Interner::new();
    let mut db = Database::new();
    let item_tree_builder = ItemTreeBuilderContext::new(&mut interner);
    let item_tree = item_tree_builder.build(&ast, &mut db);

    let mut root_ctx = RootBodyLowerContext::new();

    let mut ctx = BodyLowerContext::new(vec![]);
    let stmts = ast
        .stmts()
        .filter_map(|stmt| ctx.lower_stmt(stmt, &mut root_ctx, &db, &item_tree, &mut interner))
        .collect();
    (root_ctx, ctx, stmts, db, item_tree, interner)
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
    Call {
        callee: Symbol,
        name: Name,
        args: Vec<ExprIdx>,
    },
    Block(Block),
    Missing,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Symbol {
    Param,
    Local(ExprIdx),
    Function(FunctionIdx),
    Missing,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    stmts: Vec<Stmt>,
    pub tail: Option<ExprIdx>,
    ast: AstId<ast::Block>,
}
impl Block {
    pub fn tail<'a>(&self, ctx: &'a BodyLowerContext) -> Option<&'a Expr> {
        if let Some(tail) = self.tail {
            Some(&ctx.exprs[tail])
        } else {
            None
        }
    }
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
