use std::{collections::HashSet, fmt};

use super::{environment::Context, type_scheme::TypeSubstitution, Signature};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Monotype {
    Integer,
    Bool,
    Unit,
    Char,
    String,
    Variable(u32),
    Function(Box<Signature>),
    Never,
    Unknown,
}

impl fmt::Display for Monotype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Monotype::Integer => write!(f, "int"),
            Monotype::Bool => write!(f, "bool"),
            Monotype::Char => write!(f, "char"),
            Monotype::String => write!(f, "string"),
            Monotype::Unit => write!(f, "()"),
            Monotype::Never => write!(f, "!"),
            Monotype::Unknown => write!(f, "unknown"),
            Monotype::Variable(id) => write!(f, "{}", id),
            Monotype::Function(signature) => {
                write!(
                    f,
                    "({}) -> {}",
                    signature
                        .params
                        .iter()
                        .map(|ty| ty.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                        .to_string(),
                    signature.return_type.to_string()
                )
            }
        }
    }
}

impl Monotype {
    pub fn gen_variable(cxt: &mut Context) -> Self {
        let monotype = Self::Variable(cxt.gen_counter);
        cxt.gen_counter += 1;
        monotype
    }

    pub fn free_variables(&self) -> HashSet<u32> {
        match self {
            Monotype::Variable(id) => {
                let mut set = HashSet::new();
                set.insert(*id);

                set
            }
            Monotype::Function(signature) => {
                let mut set = HashSet::new();
                for arg in signature.params.iter() {
                    set.extend(arg.free_variables());
                }
                set.extend(signature.return_type.free_variables());
                set
            }
            _ => Default::default(),
        }
    }

    pub fn apply(&self, subst: &TypeSubstitution) -> Monotype {
        match self {
            Monotype::Integer
            | Monotype::Bool
            | Monotype::Unit
            | Monotype::Char
            | Monotype::String
            | Monotype::Never
            | Monotype::Unknown => self.clone(),
            Monotype::Variable(id) => {
                if let Some(ty) = subst.lookup(*id) {
                    ty
                } else {
                    self.clone()
                }
            }
            Monotype::Function(signagure) => Monotype::Function(
                Signature {
                    params: signagure
                        .params
                        .iter()
                        .map(|arg| arg.apply(subst))
                        .collect::<Vec<_>>(),
                    return_type: signagure.return_type.apply(subst),
                }
                .into(),
            ),
        }
    }
}
