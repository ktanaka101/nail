use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct If<'hir> {
    pub condition: &'hir hir::Expr<'hir>,
    pub then_branch: &'hir hir::Block<'hir>,
    pub else_branch: Option<&'hir hir::Block<'hir>>,
}
