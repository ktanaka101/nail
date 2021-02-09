use super::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Array,
    Boolean,
    Function,
    Hash,
    Integer,
    Char,
    String,
    Custome(String),
    Never,
}

impl From<String> for Type {
    fn from(val: String) -> Self {
        match val.as_str() {
            "Array" => Type::Array,
            "Boolean" => Type::Boolean,
            "Function" => Type::Function,
            "Hash" => Type::Hash,
            "Integer" => Type::Integer,
            "Char" => Type::Char,
            "String" => Type::String,
            "Never" => Type::Never,
            name => Type::Custome(name.to_string()),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Array => "Array",
                Type::Boolean => "Boolean",
                Type::Function => "Function",
                Type::Hash => "Hash",
                Type::Integer => "Integer",
                Type::Char => "Char",
                Type::String => "String",
                Type::Custome(name) => name,
                Type::Never => "Never",
            }
        )
    }
}
