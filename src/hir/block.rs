use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'hir> {
    pub statements: &'hir [hir::Stmt<'hir>],
}
