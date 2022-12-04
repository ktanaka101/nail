mod checker;
mod inference;

use hir::LowerResult;

pub fn lower(lower_result: &LowerResult) -> TyLowerResult {
    let _ = inference::infer(lower_result);
    todo!()
}

pub struct TyLowerResult {}
