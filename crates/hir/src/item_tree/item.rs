use std::collections::HashMap;

use crate::{
    db::{FunctionId, ModuleId, UseItemId},
    AstId, Name, ParamId, Path,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Option<Name>,
    pub ty: Type,
    pub pos: usize,
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
    pub path: Path,
    pub name: Option<Name>,
    pub params: Vec<ParamId>,
    pub param_by_name: HashMap<Name, ParamId>,
    pub return_type: Type,
    pub ast: AstId<ast::FunctionDef>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub name: Name,
    pub items: Vec<ItemDefId>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct UseItem {
    pub name: Name,
    pub path: Path,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ItemDefId {
    Function(FunctionId),
    Module(ModuleId),
    UseItem(UseItemId),
}
