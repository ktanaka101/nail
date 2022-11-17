mod database;

use la_arena::Idx;
use smol_str::SmolStr;

pub use database::Database;

pub fn lower(ast: ast::SourceFile) -> (Database, Vec<Stmt>) {
    let mut db = Database::default();
    let stmts = ast.stmts().filter_map(|stmt| db.lower_stmt(stmt)).collect();
    (db, stmts)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    VariableDef { name: SmolStr, value: Expr },
    Expr(Expr),
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
        var: SmolStr,
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
