/// ここに`salsa`データを定義します。
#[salsa::jar(db = HirDatabase)]
pub struct Jar(
    crate::Name,
    crate::NailFile,
    crate::parse_to_ast,
    crate::AstSourceFile,
    crate::build_hir_file,
    crate::HirFile,
    crate::Function,
    crate::Module,
    crate::UseItem,
    crate::NameSolutionPath,
    crate::Path,
);

/// [Jar]用のDBトレイトです。
pub trait HirDatabase: salsa::DbWithJar<Jar> {}

impl<DB> HirDatabase for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}
