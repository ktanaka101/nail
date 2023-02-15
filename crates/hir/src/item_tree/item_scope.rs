use std::collections::HashMap;

use la_arena::Idx;

use super::{FunctionIdx, ItemTree, ModuleIdx};
use crate::{db::Database, string_interner::Interner, Name};

pub type ItemScopeIdx = Idx<ItemScope>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemScope {
    function_by_name: HashMap<Name, FunctionIdx>,
    module_by_name: HashMap<Name, ModuleIdx>,
    parent: Option<Parent>,
}
impl ItemScope {
    pub fn new(parent: Option<Parent>) -> Self {
        Self {
            function_by_name: HashMap::new(),
            module_by_name: HashMap::new(),
            parent,
        }
    }

    pub fn insert(&mut self, name: Name, function: FunctionIdx) {
        self.function_by_name.insert(name, function);
    }

    pub fn insert_module(&mut self, name: Name, module: ModuleIdx) {
        self.module_by_name.insert(name, module);
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
            let function_idx = self.function_by_name.get(name).copied();
            if function_idx.is_some() {
                return function_idx;
            }
        }

        if let Some(parent) = &self.parent {
            match parent {
                Parent::TopLevel => {
                    let top_level_item_scope = &db.item_scopes[item_tree.top_level_scope];
                    top_level_item_scope.lookup(name_str, db, item_tree, interner)
                }
                Parent::SubLevel(item_scope) => {
                    let item_scope = &db.item_scopes[*item_scope];
                    item_scope.lookup(name_str, db, item_tree, interner)
                }
            }
        } else {
            None
        }
    }

    pub fn functions(&self) -> Vec<FunctionIdx> {
        let mut functions = self.function_by_name.values().copied().collect::<Vec<_>>();
        functions.sort_by_cached_key(|idx| idx.into_raw());
        functions
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Parent {
    TopLevel,
    SubLevel(ItemScopeIdx),
}
