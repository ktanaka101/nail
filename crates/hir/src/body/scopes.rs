use std::collections::HashMap;

use crate::{AstId, ExprId, Name};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ScopeType {
    TopLevel,
    SubLevel(AstId<ast::BlockExpr>),
}

#[derive(Debug)]
pub(crate) struct Scopes {
    inner: Vec<Scope>,
}
impl Scopes {
    pub(crate) fn new() -> Self {
        Self {
            inner: vec![Scope::top_level()],
        }
    }

    pub(crate) fn lookup_in_only_current_scope(&self, name: Name) -> Option<ExprId> {
        assert!(!self.inner.is_empty());

        self.inner.last().unwrap().table.get(&name).copied()
    }

    pub(crate) fn lookup(&self, name: Name) -> Option<ExprId> {
        assert!(!self.inner.is_empty());

        for scope in self.inner.iter().rev() {
            if let Some(id) = scope.table.get(&name) {
                return Some(id.to_owned());
            }
        }

        None
    }

    pub(crate) fn define(&mut self, name: Name, value: ExprId) {
        assert!(!self.inner.is_empty());

        self.inner.last_mut().unwrap().table.insert(name, value);
    }

    pub(crate) fn current_scope(&self) -> &ScopeType {
        &self.inner.last().unwrap().scope_type
    }

    pub(crate) fn enter(&mut self, block: AstId<ast::BlockExpr>) {
        self.inner.push(Scope::sub_level(block));
    }

    pub(crate) fn leave(&mut self) {
        assert!(self.inner.len() > 1);

        self.inner.pop();
    }
}

#[derive(Debug)]
struct Scope {
    table: HashMap<Name, ExprId>,
    scope_type: ScopeType,
}
impl Scope {
    fn top_level() -> Self {
        Self {
            table: HashMap::new(),
            scope_type: ScopeType::TopLevel,
        }
    }

    fn sub_level(block: AstId<ast::BlockExpr>) -> Self {
        Self {
            table: HashMap::new(),
            scope_type: ScopeType::SubLevel(block),
        }
    }
}
