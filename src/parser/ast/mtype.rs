use super::prelude::*;
use std::collections::BTreeSet;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Array,
    Boolean,
    Function,
    Hash,
    Integer,
    Char,
    String,
    Union(BTreeSet<Type>),
    Custome(String),
    Never,
}

impl Type {
    pub fn new_union(v: Vec<Type>) -> Type {
        if v.is_empty() {
            panic!("arg is empty vec.")
        }

        Type::Union(v.into_iter().collect()).compress()
    }

    fn compress(&self) -> Type {
        match self {
            Type::Union(types) => match types.len() {
                0 => panic!("types is empty."),
                1 => types.iter().next().unwrap().compress(),
                _ => {
                    let mut type_set = std::collections::BTreeSet::<Type>::new();

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

                    match type_set.len() {
                        0 => unreachable!(),
                        1 => types.iter().next().unwrap().to_owned(),
                        _ => Type::Union(type_set),
                    }
                }
            },
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
                Type::Union(types) => {
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

    #[test]
    fn test_compress() {
        let union_type = Type::new_union(vec![Type::Integer]);
        assert_eq!(union_type.compress(), Type::Integer);

        let union_type = Type::new_union(vec![Type::Integer, Type::Integer]);
        assert_eq!(union_type.compress(), Type::Integer);

        let union_type = Type::new_union(vec![Type::Integer, Type::String, Type::Integer]);
        assert_eq!(
            union_type.compress(),
            Type::new_union(vec![Type::Integer, Type::String]),
        );

        let union_type = Type::new_union(vec![
            Type::Integer,
            Type::String,
            Type::new_union(vec![Type::Integer, Type::Char]),
        ]);
        assert_eq!(
            union_type.compress(),
            Type::new_union(vec![Type::Integer, Type::String, Type::Char]),
        );

        let union_type = Type::new_union(vec![
            Type::Integer,
            Type::String,
            Type::new_union(vec![
                Type::Integer,
                Type::Char,
                Type::new_union(vec![Type::Integer, Type::Char, Type::Boolean]),
            ]),
            Type::new_union(vec![Type::Integer, Type::Char]),
        ]);
        assert_eq!(
            union_type.compress(),
            Type::new_union(vec![Type::Boolean, Type::Integer, Type::String, Type::Char]),
        );

        let union_type = Type::new_union(vec![Type::Integer, Type::new_union(vec![Type::Integer])]);
        assert_eq!(union_type.compress(), Type::Integer);

        let union_type = Type::new_union(vec![
            Type::Integer,
            Type::new_union(vec![Type::Integer, Type::new_union(vec![Type::Integer])]),
        ]);
        assert_eq!(union_type.compress(), Type::Integer);

        let union_type = Type::new_union(vec![
            Type::Integer,
            Type::new_union(vec![
                Type::Integer,
                Type::new_union(vec![Type::new_union(vec![Type::Integer])]),
            ]),
        ]);
        assert_eq!(union_type.compress(), Type::Integer);

        let union_type = Type::new_union(vec![Type::new_union(vec![Type::new_union(vec![
            Type::new_union(vec![Type::Integer]),
        ])])]);
        assert_eq!(union_type.compress(), Type::Integer);
    }

    #[test]
    fn test_type_fmt() {
        // basic
        let union_type = Type::new_union(vec![Type::Integer, Type::String]);
        assert_eq!(union_type.to_string(), "Integer | String");

        // duplicate type
        let union_type = Type::new_union(vec![Type::Integer, Type::String, Type::Integer]);
        assert_eq!(union_type.to_string(), "Integer | String");

        // nested union
        let union_type = Type::new_union(vec![
            Type::Integer,
            Type::String,
            Type::new_union(vec![Type::Char, Type::Boolean]),
        ]);
        assert_eq!(union_type.to_string(), "Boolean | Integer | Char | String");

        // duplicate nested union
        let union_type = Type::new_union(vec![
            Type::Integer,
            Type::String,
            Type::new_union(vec![Type::Integer, Type::String]),
        ]);
        assert_eq!(union_type.to_string(), "Integer | String");

        // duplicate nested union
        let union_type = Type::new_union(vec![
            Type::Integer,
            Type::String,
            Type::new_union(vec![Type::Integer, Type::Char]),
        ]);
        assert_eq!(union_type.to_string(), "Integer | Char | String");

        let union_type = Type::new_union(vec![
            Type::Integer,
            Type::String,
            Type::new_union(vec![
                Type::Integer,
                Type::Char,
                Type::new_union(vec![Type::Integer, Type::Char, Type::Boolean]),
            ]),
            Type::new_union(vec![Type::Integer, Type::Char]),
        ]);
        assert_eq!(union_type.to_string(), "Boolean | Integer | Char | String");
    }
}
