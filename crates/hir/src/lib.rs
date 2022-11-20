mod database;
mod interner;

use database::Name;
use la_arena::Idx;

pub use database::LowerContext;

pub fn lower(ast: ast::SourceFile) -> (LowerContext, Vec<Stmt>) {
    let mut db = LowerContext::new();
    let stmts = ast.stmts().filter_map(|stmt| db.lower_stmt(stmt)).collect();
    (db, stmts)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    VariableDef { name: Name, value: Idx<Expr> },
    Expr(Idx<Expr>),
}

pub type ExprIdx = Idx<Expr>;

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
        var: ExprIdx,
    },
    Missing,
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
