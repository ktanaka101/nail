use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'hir> {
    pub id: hir::HirId,
    pub kind: hir::ExprKind<'hir>,
}
