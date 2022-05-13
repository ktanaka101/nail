use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpr<'hir> {
    pub operator: hir::Operator,
    pub target: &'hir hir::Expr<'hir>,
}
