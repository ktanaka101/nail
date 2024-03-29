use std::collections::HashSet;

use indexmap::{IndexMap, IndexSet};

use super::{
    environment::{Context, VariableId},
    types::Monotype,
};
use crate::HirTyMasterDatabase;

/// Hindley-Milner型推論における型スキーマ
///
/// 型変数を持つことができる型のテンプレートのようなものです。
/// 現時点(2023-09-16)では型変数をサポートしていないため使用していません。
#[derive(Clone)]
pub struct TypeScheme {
    /// `ty`が持つ型変数の集合
    pub variables: IndexSet<VariableId>,
    /// 型
    pub ty: Monotype,
}

impl TypeScheme {
    pub fn new(ty: Monotype) -> TypeScheme {
        TypeScheme {
            variables: IndexSet::new(),
            ty,
        }
    }

    pub fn new_with_variables(ty: Monotype, variables: IndexSet<VariableId>) -> TypeScheme {
        TypeScheme { variables, ty }
    }

    #[allow(dead_code)]
    pub fn free_variables(&self, db: &dyn HirTyMasterDatabase) -> HashSet<VariableId> {
        self.ty
            .free_variables(db)
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
            replacements: IndexMap::from_iter(new_vars),
        };

        self.ty.apply(&replacement)
    }
}

#[derive(Default)]
pub struct TypeSubstitution {
    pub replacements: IndexMap<VariableId, Monotype>,
}

impl TypeSubstitution {
    pub fn lookup(&self, id: VariableId) -> Option<Monotype> {
        self.replacements.get(&id).cloned()
    }
}
