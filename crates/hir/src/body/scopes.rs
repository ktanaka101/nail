use std::collections::HashMap;

use crate::{AstId, ExprIdx, Name};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum CurrentBlock {
    Root,
    Block(AstId<ast::Block>),
}

#[derive(Debug)]
pub(crate) struct Scopes {
    inner: Vec<HashMap<Name, ExprIdx>>,
    current_block_stacks: Vec<CurrentBlock>,
}
impl Scopes {
    pub(crate) fn new() -> Self {
        let mut scopes = Self {
            inner: Vec::new(),
            current_block_stacks: vec![],
        };
        scopes.enter(CurrentBlock::Root);

        scopes
    }

    pub(crate) fn get_from_current_scope(&self, name: Name) -> Option<ExprIdx> {
        assert!(!self.inner.is_empty());

        self.inner.last().unwrap().get(&name).copied()
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

    pub(crate) fn current_block(&self) -> &CurrentBlock {
        self.current_block_stacks.last().unwrap()
    }

    pub(crate) fn enter(&mut self, current_block: CurrentBlock) {
        self.inner.push(HashMap::default());
        self.current_block_stacks.push(current_block);
    }

    pub(crate) fn leave(&mut self) {
        assert!(self.inner.len() >= 2);

        self.current_block_stacks.pop();
        self.inner.pop();
    }
}
