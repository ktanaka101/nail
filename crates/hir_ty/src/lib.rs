#[derive(Debug, Clone)]
pub struct InferenceResult {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResolvedType {
    Unknown,
    Integer,
    String,
    Char,
    Bool,
}

fn infer_body(body: Vec<hir::Stmt>, database: &hir::Database) -> InferenceResult {
    todo!()
}
