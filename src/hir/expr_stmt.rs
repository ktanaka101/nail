use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt<'hir> {
    pub expr: &'hir hir::Expr<'hir>,
}
