use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind<'hir> {
    ExprStmt(&'hir hir::ExprStmt<'hir>),
    Let(&'hir hir::Let<'hir>),
    Return(&'hir hir::Return<'hir>),
    Block(&'hir hir::Block<'hir>),
}
