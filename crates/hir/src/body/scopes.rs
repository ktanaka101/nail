use std::collections::HashMap;

use crate::{AstId, ExprIdx, Name};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ScopeType {
    TopLevel,
    SubLevel(AstId<ast::Block>),
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

    pub(crate) fn get_from_current_scope(&self, name: Name) -> Option<ExprIdx> {
        assert!(!self.inner.is_empty());

        self.inner.last().unwrap().table.get(&name).copied()
    }

    pub(crate) fn get(&self, name: Name) -> Option<ExprIdx> {
        assert!(!self.inner.is_empty());

        for scope in self.inner.iter().rev() {
            if let Some(idx) = scope.table.get(&name) {
                return Some(idx.to_owned());
            }
        }

        None
    }

    pub(crate) fn push(&mut self, name: Name, value: ExprIdx) {
        assert!(!self.inner.is_empty());

        self.inner.last_mut().unwrap().table.insert(name, value);
    }

    pub(crate) fn current_block(&self) -> &ScopeType {
        &self.inner.last().unwrap().block
    }

    pub(crate) fn enter(&mut self, block: AstId<ast::Block>) {
        self.inner.push(Scope::sub_level(block));
    }

    pub(crate) fn leave(&mut self) {
        assert!(self.inner.len() > 1);

        self.inner.pop();
    }
}

#[derive(Debug)]
struct Scope {
    table: HashMap<Name, ExprIdx>,
    block: ScopeType,
}
impl Scope {
    fn top_level() -> Self {
        Self {
            table: HashMap::new(),
            block: ScopeType::TopLevel,
        }
    }

    fn sub_level(block: AstId<ast::Block>) -> Self {
        Self {
            table: HashMap::new(),
            block: ScopeType::SubLevel(block),
        }
    }
}

// struct CurrentBlockStack(Vec<CurrentBlock>);
// impl CurrentBlockStack {
//     fn new() -> Self {
//         Self(vec![])
//     }

//     fn current(&self) -> &CurrentBlock {
//         self.0.last().unwrap()
//     }

//     fn push(&mut self, block: CurrentBlock) {
//         self.0.push(self.current());
//     }

//     fn pop(&mut self) -> &CurrentBlock {
//         assert!(self.0.is_empty());
//     }
// }
