use super::super::hir;

#[derive(Debug, PartialEq, Clone)]
pub struct Program<'hir> {
    pub id: hir::HirId,
    pub statements: &'hir [hir::Stmt<'hir>],
}
