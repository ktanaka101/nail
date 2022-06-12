use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<'hir> {
    Array(&'hir Array<'hir>),
    Boolean(&'hir Boolean),
    Char(&'hir Char),
    Call(&'hir Call<'hir>),
    Function(&'hir Function<'hir>),
    Closure(&'hir Closure<'hir>),
    Identifier(&'hir Identifier<'hir>),
    If(&'hir If<'hir>),
    Index(&'hir Index<'hir>),
    InfixExpr(&'hir InfixExpr<'hir>),
    Integer(&'hir Integer),
    PrefixExpr(&'hir PrefixExpr<'hir>),
    StringLit(&'hir StringLit),
    Hash(&'hir Hash<'hir>),
    ClosureParam(&'hir ClosureParam<'hir>),
    FunctionParam(&'hir FunctionParam<'hir>),
}
