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
            "()" => Type::Unit,
            name => Type::Custom(name.to_string()),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Array => "Array".to_string(),
                Type::Boolean => "Boolean".to_string(),
                Type::Function => "Function".to_string(),
                Type::Hash => "Hash".to_string(),
                Type::Integer => "Integer".to_string(),
                Type::Char => "Char".to_string(),
                Type::String => "String".to_string(),
                Type::Custom(name) => name.to_string(),
                Type::Unit => "()".to_string(),
                Type::Never => "Never".to_string(),
            }
        )
    }
}
