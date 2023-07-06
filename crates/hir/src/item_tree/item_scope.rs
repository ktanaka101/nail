use std::collections::HashMap;

use super::ItemTree;
use crate::{
    db::{Database, FunctionId, ItemScopeId, ModuleId, UseItemId},
    Name, Path,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemScope {
    function_by_name: HashMap<Name, FunctionId>,
    module_by_name: HashMap<Name, ModuleId>,
    use_item_by_name: HashMap<Name, UseItemId>,
    parent: Option<ParentScope>,
    name: Option<Name>,
}
impl ItemScope {
    pub fn new_with_nameless(parent: Option<ParentScope>) -> Self {
        Self {
            function_by_name: HashMap::new(),
            module_by_name: HashMap::new(),
            use_item_by_name: HashMap::new(),
            parent,
            name: None,
        }
    }

    pub fn new_with_name(parent: Option<ParentScope>, name: Name) -> Self {
        Self {
            function_by_name: HashMap::new(),
            module_by_name: HashMap::new(),
            use_item_by_name: HashMap::new(),
            parent,
            name: Some(name),
        }
    }

    pub fn path(&self, db: &Database) -> Path {
        match &self.parent {
            Some(ParentScope(parent)) => {
                let mut path = parent.lookup(db).path(db);
                if let Some(name) = self.name {
                    path.segments.push(name);
                }
                path
            }
            None => Path {
                segments: if let Some(name) = self.name {
                    vec![name]
                } else {
                    vec![]
                },
            },
        }
    }

    pub fn lookup(
        &self,
        module_paths: &[Name],
        function_name: Name,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Option<FunctionId> {
        match module_paths {
            [] => self.lookup_function(function_name, db),
            [module_path, rest @ ..] => {
                let scope = self.lookup_scope_by_module(*module_path, db, item_tree)?;
                scope.resolve_function(rest, function_name, db, item_tree)
            }
        }
    }

    fn resolve_function(
        &self,
        module_paths: &[Name],
        function_name: Name,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Option<FunctionId> {
        match module_paths {
            [] => self.function_by_name.get(&function_name).copied(),
            [module_path, rest @ ..] => {
                let scope = self.find_scope_by_module(*module_path, db, item_tree)?;
                scope.resolve_function(rest, function_name, db, item_tree)
            }
        }
    }

    fn find_scope_by_module<'a>(
        &self,
        module_path: Name,
        db: &'a Database,
        item_tree: &ItemTree,
    ) -> Option<&'a ItemScope> {
        self.module_by_name
            .get(&module_path)
            .map(|module_id| item_tree.scope_by_module(db, module_id).unwrap())
    }

    fn lookup_function(&self, function_name: Name, db: &Database) -> Option<FunctionId> {
        if let Some(function_id) = self.function_by_name.get(&function_name) {
            Some(*function_id)
        } else {
            match &self.parent {
                Some(ParentScope(scope_id)) => {
                    let item_scope = scope_id.lookup(db);
                    item_scope.lookup_function(function_name, db)
                }
                None => None,
            }
        }
    }

    fn lookup_scope_by_module<'a>(
        &self,
        module_path: Name,
        db: &'a Database,
        item_tree: &ItemTree,
    ) -> Option<&'a ItemScope> {
        if let Some(scope) = self.find_scope_by_module(module_path, db, item_tree) {
            Some(scope)
        } else {
            match &self.parent {
                Some(parent) => match parent {
                    ParentScope(scope_id) => {
                        let item_scope = scope_id.lookup(db);
                        item_scope.lookup_scope_by_module(module_path, db, item_tree)
                    }
                },
                None => None,
            }
        }
    }

    pub(crate) fn insert_function(&mut self, name: Name, function_id: FunctionId) {
        self.function_by_name.insert(name, function_id);
    }

    pub(crate) fn insert_module(&mut self, name: Name, module_id: ModuleId) {
        self.module_by_name.insert(name, module_id);
    }

    pub(crate) fn insert_use_item(&mut self, name: Name, use_item_id: UseItemId) {
        self.use_item_by_name.insert(name, use_item_id);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParentScope(ItemScopeId);
impl ParentScope {
    pub fn new(scope_id: ItemScopeId) -> Self {
        Self(scope_id)
    }

    pub fn lookup<'a>(&self, db: &'a Database) -> &'a ItemScope {
        self.0.lookup(db)
    }
}
