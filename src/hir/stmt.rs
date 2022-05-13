use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt<'hir> {
    pub id: hir::HirId,
    pub kind: hir::StmtKind<'hir>,
}
