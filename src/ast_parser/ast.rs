mod prelude {
    pub use std::convert::{TryFrom, TryInto};
    pub use std::fmt;
    pub use std::fmt::{Display, Formatter};

    pub use anyhow::{Error, Result};

    pub use crate::token::Token;

    pub use super::super::error::ParserError;
    pub use super::{
        Array, Block, Boolean, Call, Char, Closure, Expr, ExprStmt, Function, Hash, Identifier, If,
        Index, InfixExpr, Integer, Let, Node, Operator, Pair, PrefixExpr, Program, Return, Stmt,
        StringLit, Type,
    };
}

mod array;
mod block;
mod boolean;
mod call;
mod r#char;
mod closure;
mod expr;
mod expr_stmt;
mod function;
mod hash;
mod identifier;
mod r#if;
mod index;
mod infix_expr;
mod integer;
mod r#let;
mod node;
mod operator;
mod pair;
mod prefix_expr;
mod program;
mod r#return;
mod stmt;
mod string_lit;
mod r#type;

pub use self::char::Char;
pub use self::r#return::Return;
pub use array::Array;
pub use block::Block;
pub use boolean::Boolean;
pub use call::Call;
pub use closure::{Closure, ClosureParam};
pub use expr::Expr;
pub use expr_stmt::ExprStmt;
pub use function::{Function, FunctionParam};
pub use hash::Hash;
pub use identifier::Identifier;
pub use index::Index;
pub use infix_expr::InfixExpr;
pub use integer::Integer;
pub use node::Node;
pub use operator::Operator;
pub use pair::Pair;
pub use prefix_expr::PrefixExpr;
pub use program::Program;
pub use r#if::If;
pub use r#let::Let;
pub use r#type::Type;
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
