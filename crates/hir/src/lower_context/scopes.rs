use std::collections::HashMap;

use crate::{ExprIdx, Name};

#[derive(Debug)]
pub(crate) struct Scopes {
    inner: Vec<HashMap<Name, ExprIdx>>,
}
impl Scopes {
    pub(crate) fn new() -> Self {
        let mut scopes = Self { inner: Vec::new() };
        scopes.enter();

        scopes
    }

    pub(crate) fn get(&self, name: Name) -> Option<ExprIdx> {
        assert!(!self.inner.is_empty());

        for scope in self.inner.iter().rev() {
            if let Some(idx) = scope.get(&name) {
                return Some(idx.to_owned());
            }
        }

        None
    }

    pub(crate) fn push(&mut self, name: Name, value: ExprIdx) {
        assert!(!self.inner.is_empty());

        self.inner.last_mut().unwrap().insert(name, value);
    }

    pub(crate) fn enter(&mut self) {
        self.inner.push(HashMap::default());
    }
}
