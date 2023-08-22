use crate::input::NailFile;

#[salsa::tracked]
pub struct AstSourceFile {
    /** Nailファイル */
    pub file: NailFile,

    /** AST */
    #[return_ref]
    pub source: ast::SourceFile,
}
