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
    Union(Vec<Type>),
    Custome(String),
    Never,
}

impl Type {
    fn compress(&self) -> Type {
        match self {
            Type::Union(types) => {
                let mut type_set = std::collections::HashSet::<Type>::new();

                types.iter().for_each(|ty| match ty.compress() {
                    Type::Union(types) => {
                        types.into_iter().for_each(|ty| {
                            type_set.insert(ty.compress());
                        });
                    }
                    other => {
                        type_set.insert(other.compress());
                    }
                });

                Type::Union(type_set.into_iter().collect::<Vec<_>>())
            }
            other => other.clone(),
        }
    }
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
            match self.compress() {
                Type::Array => "Array".to_string(),
                Type::Boolean => "Boolean".to_string(),
                Type::Function => "Function".to_string(),
                Type::Hash => "Hash".to_string(),
                Type::Integer => "Integer".to_string(),
                Type::Char => "Char".to_string(),
                Type::String => "String".to_string(),
                Type::Union(mut types) => {
                    types.sort();
                    let types = types.iter().map(|ty| ty.to_string()).collect::<Vec<_>>();
                    types.join(" | ")
                }
                Type::Custome(name) => name,
                Type::Never => "Never".to_string(),
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_eq_union(expected_union: Type, actual_union: Type) {
        match (expected_union, actual_union) {
            (Type::Union(mut expected), Type::Union(mut actual)) => {
                expected.sort();
                actual.sort();
                assert_eq!(expected, actual);
            }
            (expected, actual) => panic!("expected: {:?}, actual: {:?}", expected, actual),
        }
    }

    #[test]
    fn test_compress() {
        let union_type = Type::Union(vec![Type::Integer, Type::Integer]);
        assert_eq_union(union_type.compress(), Type::Union(vec![Type::Integer]));

        let union_type = Type::Union(vec![Type::Integer, Type::String, Type::Integer]);
        assert_eq_union(
            union_type.compress(),
            Type::Union(vec![Type::Integer, Type::String]),
        );

        let union_type = Type::Union(vec![Type::Integer, Type::Union(vec![Type::Integer])]);
        assert_eq_union(union_type.compress(), Type::Union(vec![Type::Integer]));

        let union_type = Type::Union(vec![
            Type::Integer,
            Type::String,
            Type::Union(vec![Type::Integer, Type::Char]),
        ]);
        assert_eq_union(
            union_type.compress(),
            Type::Union(vec![Type::Integer, Type::String, Type::Char]),
        );

        let union_type = Type::Union(vec![
            Type::Integer,
            Type::String,
            Type::Union(vec![
                Type::Integer,
                Type::Char,
                Type::Union(vec![Type::Integer, Type::Char, Type::Boolean]),
            ]),
            Type::Union(vec![Type::Integer, Type::Char]),
        ]);
        assert_eq_union(
            union_type.compress(),
            Type::Union(vec![Type::Boolean, Type::Integer, Type::String, Type::Char]),
        );
    }

    #[test]
    fn test_type_fmt() {
        // basic
        let union_type = Type::Union(vec![Type::Integer, Type::String]);
        assert_eq!(union_type.to_string(), "Integer | String");

        // duplicate type
        let union_type = Type::Union(vec![Type::Integer, Type::String, Type::Integer]);
        assert_eq!(union_type.to_string(), "Integer | String");

        // nested union
        let union_type = Type::Union(vec![
            Type::Integer,
            Type::String,
            Type::Union(vec![Type::Char, Type::Boolean]),
        ]);
        assert_eq!(union_type.to_string(), "Boolean | Integer | Char | String");

        // duplicate nested union
        let union_type = Type::Union(vec![
            Type::Integer,
            Type::String,
            Type::Union(vec![Type::Integer, Type::String]),
        ]);
        assert_eq!(union_type.to_string(), "Integer | String");

        // duplicate nested union
        let union_type = Type::Union(vec![
            Type::Integer,
            Type::String,
            Type::Union(vec![Type::Integer, Type::Char]),
        ]);
        assert_eq!(union_type.to_string(), "Integer | Char | String");

        let union_type = Type::Union(vec![
            Type::Integer,
            Type::String,
            Type::Union(vec![
                Type::Integer,
                Type::Char,
                Type::Union(vec![Type::Integer, Type::Char, Type::Boolean]),
            ]),
            Type::Union(vec![Type::Integer, Type::Char]),
        ]);
        assert_eq!(union_type.to_string(), "Boolean | Integer | Char | String");
    }
}
