use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct Return<'hir> {
    pub expr: &'hir hir::Expr<'hir>,
}
