mod prelude {
    pub use std::convert::{TryFrom, TryInto};
    pub use std::fmt;
    pub use std::fmt::{Display, Formatter};

    pub use anyhow::{Error, Result};

    pub use crate::token::Token;

    pub use super::super::error::ParserError;
    pub use super::{
        Array, Block, Boolean, Call, Char, Expr, ExprStmt, Function, Hash, Identifier, If, Index,
        InfixExpr, Integer, Let, MacroLit, Node, Operator, Pair, PrefixExpr, Program, Return, Stmt,
        StringLit, Type,
    };
}

mod array;
mod block;
mod boolean;
mod call;
mod expr;
mod expr_stmt;
mod function;
mod hash;
mod identifier;
mod index;
mod infix_expr;
mod integer;
mod macro_lit;
mod mchar;
mod mif;
mod mlet;
mod mreturn;
mod mtype;
mod node;
mod operator;
mod pair;
mod prefix_expr;
mod program;
mod stmt;
mod string_lit;

pub use array::Array;
pub use block::Block;
pub use boolean::Boolean;
pub use call::Call;
pub use expr::Expr;
pub use expr_stmt::ExprStmt;
pub use function::{Function, FunctionType};
pub use hash::Hash;
pub use identifier::Identifier;
pub use index::Index;
pub use infix_expr::InfixExpr;
pub use integer::Integer;
pub use macro_lit::MacroLit;
pub use mchar::Char;
pub use mif::If;
pub use mlet::Let;
pub use mreturn::Return;
pub use mtype::Type;
pub use node::Node;
pub use operator::Operator;
pub use pair::Pair;
pub use prefix_expr::PrefixExpr;
pub use program::Program;
pub use stmt::Stmt;
pub use string_lit::StringLit;

use crate::token;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Tokens {
    tokens: Vec<token::Token>,
}

impl From<Vec<token::Token>> for Tokens {
    fn from(tokens: Vec<token::Token>) -> Self {
        Tokens { tokens }
    }
}
