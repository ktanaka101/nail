use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Type<'hir> {
    pub id: HirId,
    pub kind: &'hir TypeKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Infer,
}
