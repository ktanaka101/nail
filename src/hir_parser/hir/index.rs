use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct Index<'hir> {
    pub target: &'hir hir::Expr<'hir>,
    pub index: &'hir hir::Expr<'hir>,
}
