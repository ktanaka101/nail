use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier<'hir> {
    pub name: &'hir Symbol,
    pub resolved_expr: Option<&'hir hir::Expr<'hir>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(pub String);
