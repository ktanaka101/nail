use std::collections::HashMap;

use la_arena::Idx;

use crate::{AstId, Name};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Option<Name>,
    pub ty: Type,
    pub pos: usize,
}
pub type ParamIdx = Idx<Param>;

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
    pub params: Vec<ParamIdx>,
    pub param_by_name: HashMap<Name, ParamIdx>,
    pub return_type: Type,
    pub ast: AstId<ast::FunctionDef>,
}
pub type FunctionIdx = Idx<Function>;

#[derive(Debug, PartialEq, Eq)]
pub enum Item {
    Function(FunctionIdx),
}
