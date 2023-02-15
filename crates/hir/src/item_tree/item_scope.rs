use std::collections::HashMap;

use super::ItemTree;
use crate::{
    db::{Database, FunctionId, ItemScopeId, ModuleId},
    string_interner::Interner,
    Name,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemScope {
    function_by_name: HashMap<Name, FunctionId>,
    module_by_name: HashMap<Name, ModuleId>,
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

    pub fn insert(&mut self, name: Name, function: FunctionId) {
        self.function_by_name.insert(name, function);
    }

    pub fn insert_module(&mut self, name: Name, module: ModuleId) {
        self.module_by_name.insert(name, module);
    }

    pub fn lookup(
        &self,
        name_str: &str,
        db: &Database,
        item_tree: &ItemTree,
        interner: &Interner,
    ) -> Option<FunctionId> {
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
                    let top_level_item_scope = item_tree.top_level_scope.lookup(db);
                    top_level_item_scope.lookup(name_str, db, item_tree, interner)
                }
                Parent::SubLevel(item_scope_id) => {
                    let item_scope = item_scope_id.lookup(db);
                    item_scope.lookup(name_str, db, item_tree, interner)
                }
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Parent {
    TopLevel,
    SubLevel(ItemScopeId),
}
