use std::collections::HashMap;

use super::ItemTree;
use crate::{
    db::{Database, FunctionId, ItemScopeId, ModuleId},
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

    pub fn lookup(
        &self,
        module_path: &[Name],
        function_name: &Name,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Option<FunctionId> {
        match module_path {
            [] => {
                let function_id = self.function_by_name.get(function_name).copied();
                if function_id.is_some() {
                    return function_id;
                }

                if let Some(parent) = &self.parent {
                    match parent {
                        Parent::TopLevel => {
                            let top_level_item_scope = item_tree.top_level_scope.lookup(db);
                            top_level_item_scope.lookup(&[], function_name, db, item_tree)
                        }
                        Parent::SubLevel(item_scope_id) => {
                            let item_scope = item_scope_id.lookup(db);
                            item_scope.lookup(&[], function_name, db, item_tree)
                        }
                    }
                } else {
                    None
                }
            }
            [module_path, rest @ ..] => {
                let module_id = self.module_by_name.get(module_path)?;
                let scope = item_tree.scope_by_module(db, module_id).unwrap();
                scope.lookup(rest, function_name, db, item_tree)
            }
        }
    }

    pub(crate) fn insert_function(&mut self, name: Name, function_id: FunctionId) {
        self.function_by_name.insert(name, function_id);
    }

    pub(crate) fn insert_module(&mut self, name: Name, module_id: ModuleId) {
        self.module_by_name.insert(name, module_id);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Parent {
    TopLevel,
    SubLevel(ItemScopeId),
}
