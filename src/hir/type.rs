use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type<'hir> {
    pub id: HirId,
    pub kind: &'hir TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Infer,
}
