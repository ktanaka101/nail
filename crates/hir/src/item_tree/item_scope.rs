use std::collections::HashMap;

use super::ItemTree;
use crate::{
    db::{Database, FunctionId, ItemScopeId, ModuleId},
    Name, Path,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemScope {
    function_by_name: HashMap<Name, FunctionId>,
    module_by_name: HashMap<Name, ModuleId>,
    parent: Option<Parent>,
    name: Option<Name>,
}
impl ItemScope {
    pub fn new(parent: Option<Parent>) -> Self {
        Self {
            function_by_name: HashMap::new(),
            module_by_name: HashMap::new(),
            parent,
            name: None,
        }
    }

    pub fn new_with_name(parent: Option<Parent>, name: Name) -> Self {
        Self {
            function_by_name: HashMap::new(),
            module_by_name: HashMap::new(),
            parent,
            name: Some(name),
        }
    }

    pub fn path(&self, db: &Database) -> Path {
        match &self.parent {
            Some(parent) => match parent {
                Parent::TopLevel => Path {
                    segments: if let Some(name) = self.name {
                        vec![name]
                    } else {
                        vec![]
                    },
                },
                Parent::SubLevel(parent) => {
                    let mut path = parent.lookup(db).path(db);
                    if let Some(name) = self.name {
                        path.segments.push(name);
                    }
                    path
                }
            },
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
            [] => self.lookup_function(function_name, db, item_tree),
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

    fn lookup_function(
        &self,
        function_name: Name,
        db: &Database,
        item_tree: &ItemTree,
    ) -> Option<FunctionId> {
        if let Some(function_id) = self.function_by_name.get(&function_name) {
            Some(*function_id)
        } else {
            match &self.parent {
                Some(parent) => match parent {
                    Parent::TopLevel => {
                        let top_level_item_scope = item_tree.top_level_scope.lookup(db);
                        top_level_item_scope.lookup_function(function_name, db, item_tree)
                    }
                    Parent::SubLevel(item_scope_id) => {
                        let item_scope = item_scope_id.lookup(db);
                        item_scope.lookup_function(function_name, db, item_tree)
                    }
                },
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
                    Parent::TopLevel => {
                        let top_level_item_scope = item_tree.top_level_scope.lookup(db);
                        top_level_item_scope.lookup_scope_by_module(module_path, db, item_tree)
                    }
                    Parent::SubLevel(item_scope_id) => {
                        let item_scope = item_scope_id.lookup(db);
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Parent {
    TopLevel,
    SubLevel(ItemScopeId),
}
