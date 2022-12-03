mod body;
mod db;
mod item_tree;
pub mod string_interner;

use std::marker::PhantomData;

use ast::Ast;
pub use body::{BodyLowerContext, SharedBodyLowerContext};
pub use db::Database;
use item_tree::ItemTreeBuilderContext;
pub use item_tree::{FunctionIdx, ItemTree, Type};
use la_arena::Idx;
use string_interner::{Interner, Key};
use syntax::SyntaxNodePtr;

#[derive(Debug)]
pub struct LowerResult {
    pub shared_ctx: SharedBodyLowerContext,
    pub root_ctx: BodyLowerContext,
    pub stmts: Vec<Stmt>,
    pub db: Database,
    pub item_tree: ItemTree,
    pub interner: Interner,
}

pub fn lower(ast: ast::SourceFile) -> LowerResult {
    let mut interner = Interner::new();
    let mut db = Database::new();
    let item_tree_builder = ItemTreeBuilderContext::new(&mut interner);
    let item_tree = item_tree_builder.build(&ast, &mut db);

    let mut shared_ctx = SharedBodyLowerContext::new();

    let mut root_ctx = BodyLowerContext::new(vec![]);
    let stmts = ast
        .stmts()
        .filter_map(|stmt| {
            root_ctx.lower_stmt(stmt, &mut shared_ctx, &db, &item_tree, &mut interner)
        })
        .collect();

    LowerResult {
        shared_ctx,
        root_ctx,
        stmts,
        db,
        item_tree,
        interner,
    }
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
        signature: FunctionIdx,
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
    },
    Call {
        callee: Symbol,
        args: Vec<ExprIdx>,
    },
    Block(Block),
    Missing,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Symbol {
    Param { name: Name },
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
