use std::ops::Sub;

use indexmap::{IndexMap, IndexSet};

use super::{type_scheme::TypeScheme, types::Monotype};
use crate::HirTyMasterDatabase;

/// Hindley-Milner型システムにおける型環境
#[derive(Default)]
pub struct Environment {
    bindings: IndexMap<hir::ExprId, TypeScheme>,
}

/// 型変数のID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(u32);
impl std::fmt::Display for VariableId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Default)]
pub struct Context {
    gen_counter: u32,
}
impl Context {
    /// IDを生成します。IDは生成時に自動的にインクリメントされます。
    pub(crate) fn gen_id(&mut self) -> VariableId {
        let id = self.gen_counter;
        self.gen_counter += 1;

        VariableId(id)
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            bindings: IndexMap::new(),
        }
    }

    #[allow(dead_code)]
    fn free_variables(&self, db: &dyn HirTyMasterDatabase) -> IndexSet<VariableId> {
        let mut union = IndexSet::new();
        for type_scheme in self.bindings.values() {
            union.extend(type_scheme.free_variables(db));
        }

        union
    }

    pub(crate) fn with(&self) -> Environment {
        let mut copy = IndexMap::new();
        // FIXME: clone かつサイズが不定なので遅いかも。
        copy.extend(self.bindings.clone());

        Environment { bindings: copy }
    }

    pub(crate) fn get(&self, expr: &hir::ExprId) -> Option<&TypeScheme> {
        self.bindings.get(expr)
    }

    pub(crate) fn insert(&mut self, expr: hir::ExprId, ty_scheme: TypeScheme) {
        self.bindings.insert(expr, ty_scheme);
    }

    #[allow(dead_code)]
    fn generalize(&self, ty: &Monotype, db: &dyn HirTyMasterDatabase) -> TypeScheme {
        TypeScheme::new_with_variables(*ty, ty.free_variables(db).sub(&self.free_variables(db)))
    }
}
