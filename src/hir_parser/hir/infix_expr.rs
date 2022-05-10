use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct InfixExpr<'hir> {
    pub left: &'hir hir::Expr<'hir>,
    pub operator: hir::Operator,
    pub right: &'hir hir::Expr<'hir>,
}
