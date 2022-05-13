use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct Let<'hir> {
    pub name: &'hir hir::Symbol,
    pub value: &'hir hir::Expr<'hir>,
}
