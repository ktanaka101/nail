use std::collections::HashMap;

use la_arena::Idx;

use super::{BlockAstId, FunctionIdx, ItemTree};
use crate::{db::Database, string_interner::Interner, Name};

pub type ItemScopeIdx = Idx<ItemScope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemScope {
    functions: HashMap<Name, FunctionIdx>,
    parent: Option<Parent>,
}
impl ItemScope {
    pub fn new(parent: Option<Parent>) -> Self {
        Self {
            functions: HashMap::new(),
            parent,
        }
    }

    pub fn insert(&mut self, name: Name, function: FunctionIdx) {
        self.functions.insert(name, function);
    }

    pub fn lookup(
        &self,
        name_str: &str,
        db: &Database,
        item_tree: &ItemTree,
        interner: &Interner,
    ) -> Option<FunctionIdx> {
        let function_name_key = interner.get_key(name_str);
        if let Some(function_name_key) = function_name_key {
            let name = &Name::from_key(function_name_key);
            let function_idx = self.functions.get(name).copied();
            if function_idx.is_some() {
                return function_idx;
            }
        }

        if let Some(parent) = &self.parent {
            match parent {
                Parent::Root => {
                    let root_item_scope = &db.item_scopes[item_tree.root_scope];
                    root_item_scope.lookup(name_str, db, item_tree, interner)
                }
                Parent::Block(block) => {
                    let block_item_scope = &db.item_scopes[item_tree.scope[block]];
                    block_item_scope.lookup(name_str, db, item_tree, interner)
                }
            }
        } else {
            None
        }
    }

    pub fn functions(&self) -> Vec<FunctionIdx> {
        let mut functions = self.functions.values().copied().collect::<Vec<_>>();
        functions.sort_by_cached_key(|idx| idx.into_raw());
        functions
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Parent {
    Root,
    Block(BlockAstId),
}
