use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct Call<'hir> {
    pub func: &'hir hir::Expr<'hir>,
    pub args: &'hir [hir::Expr<'hir>],
}
