use std::{collections::HashSet, fmt};

use super::{environment::Context, type_scheme::TypeSubstitution};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Monotype {
    Integer,
    Bool,
    Unit,
    Char,
    String,
    Variable(u32),
    Function {
        from: Box<Monotype>,
        to: Box<Monotype>,
    },
    Never,
    Unknown,
}

impl fmt::Display for Monotype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Monotype::Integer => write!(f, "integer"),
            Monotype::Bool => write!(f, "bool"),
            Monotype::Char => write!(f, "char"),
            Monotype::String => write!(f, "string"),
            Monotype::Unit => write!(f, "()"),
            Monotype::Never => write!(f, "!"),
            Monotype::Unknown => write!(f, "unknown"),
            Monotype::Variable(id) => write!(f, "{}", id),
            Monotype::Function { from, to } => {
                if let Monotype::Function { from, .. } = from.as_ref() {
                    write!(f, "({}) -> {}", from.to_string(), to.to_string())
                } else {
                    write!(f, "{} -> {}", from.to_string(), to.to_string())
                }
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
            Monotype::Function { from, to } => from
                .free_variables()
                .union(&to.free_variables())
                .cloned()
                .collect(),
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
            Monotype::Function { from, to } => Monotype::Function {
                from: Box::new(from.apply(subst)),
                to: Box::new(to.apply(subst)),
            },
        }
    }
}
