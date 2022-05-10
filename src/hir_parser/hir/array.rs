use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct Array<'hir> {
    pub elements: &'hir [hir::Expr<'hir>],
}
