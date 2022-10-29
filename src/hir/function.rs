use super::super::hir;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionParam<'hir> {
    pub id: hir::HirId,
    pub r#type: &'hir hir::Type<'hir>,
    pub name: &'hir hir::Symbol,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'hir> {
    pub params: &'hir [FunctionParam<'hir>],
    pub body: &'hir hir::Block<'hir>,
    pub name: &'hir hir::Symbol,
}
