use std::{
    collections::{HashMap, HashSet},
    iter::FromIterator,
};

use super::{environment::Context, types::Monotype};

#[derive(Clone)]
pub struct TypeScheme {
    pub variables: HashSet<u32>,
    pub ty: Monotype,
}

impl TypeScheme {
    pub fn new(ty: Monotype) -> TypeScheme {
        TypeScheme {
            variables: HashSet::new(),
            ty,
        }
    }

    #[allow(dead_code)]
    pub fn free_variables(&self) -> HashSet<u32> {
        self.ty
            .free_variables()
            .into_iter()
            .filter(|var| !self.variables.contains(var))
            .collect()
    }

    /// 具体的な型を生成する
    pub fn instantiate(&self, cxt: &mut Context) -> Monotype {
        let new_vars = self
            .variables
            .iter()
            .map(|v| (*v, Monotype::gen_variable(cxt)));

        let replacement = TypeSubstitution {
            replacements: HashMap::from_iter(new_vars),
        };

        self.ty.apply(&replacement)
    }
}

#[derive(Default)]
pub struct TypeSubstitution {
    pub replacements: HashMap<u32, Monotype>,
}

impl TypeSubstitution {
    pub fn lookup(&self, id: u32) -> Option<Monotype> {
        self.replacements.get(&id).cloned()
    }
}
