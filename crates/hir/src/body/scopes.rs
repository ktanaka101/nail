//! 関数ボディのスコープの実装です。

use std::collections::HashMap;

use crate::{ExprId, Name};

/// スコープ種別
///
/// 関数内のトップレベルが[ScopeType::TopLevel]です。
/// 関数ボディのブロックに対応するスコープです。
///
/// 関数ボディ内の各ブロックが[ScopeType::SubLevel]です。
/// 例えば、if文のブロックやwhile文のブロックなどです。
/// スコープを持つブロックは、必ず[ExprScope]と対応しています。
///
/// 関数内に関数を定義した場合は、その定義した関数は[ScopeType::TopLevel]となります。
/// これは、関数内に関数から親の関数にアクセスできないため、独立したスコープを持つためです。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ScopeType {
    TopLevel,
    SubLevel(ast::BlockExpr),
}

#[derive(Debug)]
pub(crate) struct ExprScopes {
    inner: Vec<ExprScope>,
}
impl ExprScopes {
    #[must_use]
    pub(crate) fn new() -> Self {
        Self {
            inner: vec![ExprScope::top_level()],
        }
    }

    #[must_use]
    pub(crate) fn lookup_in_only_current_scope(&self, name: Name) -> Option<ExprId> {
        assert!(!self.inner.is_empty());

        self.inner.last().unwrap().table.get(&name).copied()
    }

    #[must_use]
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

    pub(crate) fn enter(&mut self, block: ast::BlockExpr) {
        self.inner.push(ExprScope::sub_level(block));
    }

    pub(crate) fn leave(&mut self) {
        assert!(self.inner.len() > 1);

        self.inner.pop();
    }
}

#[derive(Debug)]
struct ExprScope {
    table: HashMap<Name, ExprId>,
    _scope_type: ScopeType,
}
impl ExprScope {
    #[must_use]
    fn top_level() -> Self {
        Self {
            table: HashMap::new(),
            _scope_type: ScopeType::TopLevel,
        }
    }

    #[must_use]
    fn sub_level(block: ast::BlockExpr) -> Self {
        Self {
            table: HashMap::new(),
            _scope_type: ScopeType::SubLevel(block),
        }
    }
}
