use typed_arena;

use super::hir;

#[derive(Default)]
pub struct Arena<'hir> {
    array: typed_arena::Arena<hir::Array<'hir>>,
    block: typed_arena::Arena<hir::Block<'hir>>,
    boolean: typed_arena::Arena<hir::Boolean>,
    call: typed_arena::Arena<hir::Call<'hir>>,
    r#char: typed_arena::Arena<hir::Char>,
    expr: typed_arena::Arena<hir::Expr<'hir>>,
    expr_stmt: typed_arena::Arena<hir::ExprStmt<'hir>>,
    function: typed_arena::Arena<hir::Function<'hir>>,
    hash: typed_arena::Arena<hir::Hash<'hir>>,
    hash_pair: typed_arena::Arena<hir::HashPair<'hir>>,
    identifier: typed_arena::Arena<hir::Identifier<'hir>>,
    r#if: typed_arena::Arena<hir::If<'hir>>,
    index: typed_arena::Arena<hir::Index<'hir>>,
    infix_expr: typed_arena::Arena<hir::InfixExpr<'hir>>,
    integer: typed_arena::Arena<hir::Integer>,
    r#let: typed_arena::Arena<hir::Let<'hir>>,
    prefix_expr: typed_arena::Arena<hir::PrefixExpr<'hir>>,
    program: typed_arena::Arena<hir::Program<'hir>>,
    r#return: typed_arena::Arena<hir::Return<'hir>>,
    stmt: typed_arena::Arena<hir::Stmt<'hir>>,
    string_lit: typed_arena::Arena<hir::StringLit>,
    symbol: typed_arena::Arena<hir::Symbol>,
    function_param: typed_arena::Arena<hir::FunctionParam<'hir>>,
    r#type: typed_arena::Arena<hir::Type<'hir>>,
    type_kind: typed_arena::Arena<hir::TypeKind>,
    closure_param: typed_arena::Arena<hir::ClosureParam<'hir>>,
    closure: typed_arena::Arena<hir::Closure<'hir>>,
}

impl<'hir> Arena<'hir> {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn alloc<T: ArenaAllocatable<'hir>>(&'hir self, value: T) -> &'hir mut T {
        value.allocate_on(self)
    }

    #[allow(clippy::mut_from_ref)]
    pub fn alloc_from_iter<T: ArenaAllocatable<'hir>>(
        &'hir self,
        iter: impl ::std::iter::IntoIterator<Item = T>,
    ) -> &'hir mut [T] {
        T::allocate_from_iter(self, iter)
    }
}

pub trait ArenaAllocatable<'hir>: Sized {
    #[allow(clippy::mut_from_ref)]
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self;
    #[allow(clippy::mut_from_ref)]
    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self];
}

impl<'hir> ArenaAllocatable<'hir> for hir::Array<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.array.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.array.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Block<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.block.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.block.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Boolean {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.boolean.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.boolean.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Call<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.call.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.call.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Char {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.char.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.char.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::ExprStmt<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.expr_stmt.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.expr_stmt.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Expr<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.expr.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.expr.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Function<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.function.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.function.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Hash<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.hash.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.hash.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Identifier<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.identifier.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.identifier.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::If<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.r#if.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.r#if.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Index<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.index.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.index.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::InfixExpr<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.infix_expr.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.infix_expr.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Integer {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.integer.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.integer.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Let<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.r#let.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.r#let.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::PrefixExpr<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.prefix_expr.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.prefix_expr.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Program<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.program.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.program.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Return<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.r#return.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.r#return.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Stmt<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.stmt.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.stmt.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::StringLit {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.string_lit.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.string_lit.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Symbol {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.symbol.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.symbol.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::FunctionParam<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.function_param.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.function_param.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::HashPair<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.hash_pair.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.hash_pair.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Type<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.r#type.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.r#type.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::TypeKind {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.type_kind.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.type_kind.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::ClosureParam<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.closure_param.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.closure_param.alloc_extend(iter.into_iter())
    }
}

impl<'hir> ArenaAllocatable<'hir> for hir::Closure<'hir> {
    fn allocate_on(self, arena: &'hir Arena<'hir>) -> &'hir mut Self {
        arena.closure.alloc(self)
    }

    fn allocate_from_iter(
        arena: &'hir Arena<'hir>,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'hir mut [Self] {
        arena.closure.alloc_extend(iter.into_iter())
    }
}
