use indexmap::IndexSet;

use super::{
    environment::{Context, VariableId},
    type_scheme::TypeSubstitution,
    Signature,
};
use crate::HirTyMasterDatabase;

/// 単一の型
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Monotype {
    Integer,
    Bool,
    Unit,
    Char,
    String,
    Variable(VariableId),
    Function(Signature),
    Never,
    Unknown,
}

impl Monotype {
    pub(crate) fn gen_variable(cxt: &mut Context) -> Self {
        Monotype::Variable(cxt.gen_id())
    }

    pub(crate) fn free_variables(&self, db: &dyn HirTyMasterDatabase) -> IndexSet<VariableId> {
        match self {
            Monotype::Variable(id) => {
                let mut set = IndexSet::new();
                set.insert(*id);

                set
            }
            Monotype::Function(signature) => {
                let mut set = IndexSet::new();
                for arg in signature.params(db).iter() {
                    set.extend(arg.free_variables(db));
                }
                set.extend(signature.return_type(db).free_variables(db));
                set
            }
            _ => Default::default(),
        }
    }

    pub(crate) fn apply(&self, subst: &TypeSubstitution) -> Monotype {
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
            // 関数シグネチャに自由変数を持たないので何もしない
            Monotype::Function(_signagure) => self.clone(),
        }
    }
}
