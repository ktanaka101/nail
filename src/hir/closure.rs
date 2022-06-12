use super::super::hir;

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureParam<'hir> {
    pub id: hir::HirId,
    pub r#type: &'hir hir::Type<'hir>,
    pub name: &'hir hir::Symbol,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure<'hir> {
    pub params: &'hir [ClosureParam<'hir>],
    pub body: &'hir hir::Block<'hir>,
}
