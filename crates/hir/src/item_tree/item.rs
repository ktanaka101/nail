use la_arena::Idx;

use crate::{AstId, Name};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Option<Name>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    String,
    Char,
    Boolean,
    Unit,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Option<Name>,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub ast: AstId<ast::FunctionDef>,
}
pub type FunctionIdx = Idx<Function>;
