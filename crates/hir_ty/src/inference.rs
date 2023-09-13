mod environment;
mod type_scheme;
mod type_unifier;
mod types;

use std::collections::HashMap;

use environment::{Environment, InferBody};
pub use environment::{InferenceBodyResult, InferenceError, InferenceResult, Signature};
pub use type_scheme::TypeScheme;
pub use types::Monotype;

pub fn infer_pods(db: &dyn hir::HirMasterDatabase, pods: &hir::Pods) -> InferenceResult {
    let mut signature_by_function = HashMap::<hir::Function, Signature>::new();
    for (hir_file, function) in pods.root_pod.all_functions(db) {
        let signature = lower_signature(db, hir_file, function);
        signature_by_function.insert(function, signature);
    }

    let mut body_result_by_function = HashMap::<hir::Function, InferenceBodyResult>::new();
    for (hir_file, function) in pods.root_pod.all_functions(db) {
        let env = Environment::new();
        let infer_body = InferBody::new(db, hir_file, function, &signature_by_function, env);
        let infer_body_result = infer_body.infer_body();

        body_result_by_function.insert(function, infer_body_result);
    }

    InferenceResult {
        signature_by_function,
        inference_body_result_by_function: body_result_by_function,
    }
}

fn lower_signature(
    db: &dyn hir::HirMasterDatabase,
    hir_file: hir::HirFile,
    function: hir::Function,
) -> Signature {
    let params = function
        .params(db)
        .iter()
        .map(|param| {
            let param_data = param.data(hir_file.db(db));
            lower_type(&param_data.ty)
        })
        .collect::<Vec<_>>();

    let return_type = lower_type(&function.return_type(db));

    Signature {
        params,
        return_type,
    }
}

fn lower_type(ty: &hir::Type) -> Monotype {
    match ty {
        hir::Type::Integer => Monotype::Integer,
        hir::Type::String => Monotype::String,
        hir::Type::Char => Monotype::Char,
        hir::Type::Boolean => Monotype::Bool,
        hir::Type::Unit => Monotype::Unit,
        hir::Type::Unknown => Monotype::Unknown,
    }
}
