#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type<'hir> {
    Array(&'hir Type<'hir>),
    Boolean,
    Integer,
    Char,
    String,
    Hash(&'hir Type<'hir>, &'hir Type<'hir>),
    Function(&'hir [Type<'hir>], &'hir Type<'hir>),
    Custom(String),
    Unit,
    Never,
    Infer,
    Unknown,
}
