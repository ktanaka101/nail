use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct HashPair<'hir> {
    pub key: &'hir hir::Expr<'hir>,
    pub value: &'hir hir::Expr<'hir>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Hash<'hir> {
    pub pairs: &'hir [HashPair<'hir>],
}
