mod array;
mod block;
mod boolean;
mod call;
mod r#char;
mod closure;
mod expr;
mod expr_kind;
mod expr_stmt;
mod function;
mod hash;
mod identifier;
mod r#if;
mod index;
mod infix_expr;
mod integer;
mod r#let;
mod operator;
mod prefix_expr;
mod program;
mod r#return;
mod stmt;
mod stmt_kind;
mod string_lit;
mod r#type;

#[derive(Clone, Debug, PartialEq, Eq, std::hash::Hash)]
pub struct HirId(u64);
impl HirId {
    pub fn new(id: u64) -> Self {
        Self(id)
    }

    pub fn id(&self) -> u64 {
        self.0
    }
}

pub use self::char::Char;
pub use array::Array;
pub use block::Block;
pub use boolean::Boolean;
pub use call::Call;
pub use closure::{Closure, ClosureParam};
pub use expr::Expr;
pub use expr_kind::ExprKind;
pub use expr_stmt::ExprStmt;
pub use function::{Function, FunctionParam};
pub use hash::{Hash, HashPair};
pub use identifier::{Identifier, Symbol};
pub use index::Index;
pub use infix_expr::InfixExpr;
pub use integer::Integer;
pub use operator::Operator;
pub use prefix_expr::PrefixExpr;
pub use program::Program;
pub use r#if::If;
pub use r#let::Let;
pub use r#return::Return;
pub use r#type::{Type, TypeKind};
pub use stmt::Stmt;
pub use stmt_kind::StmtKind;
pub use string_lit::StringLit;
