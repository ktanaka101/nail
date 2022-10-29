use super::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Array,
    Boolean,
    Function,
    Hash,
    Integer,
    Char,
    String,
    Custom(String),
    Unit,
    Never,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Array => "array",
                Type::Boolean => "bool",
                Type::Function => "Fn",
                Type::Hash => "hash",
                Type::Integer => "int",
                Type::Char => "char",
                Type::String => "string",
                Type::Custom(name) => name,
                Type::Unit => "unit",
                Type::Never => "never",
            }
        )
    }
}
