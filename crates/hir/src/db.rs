/// HIRの全体のデータベースです。
///
/// ここに`salsa`データを定義します。
#[salsa::jar(db = HirMasterDatabase)]
pub struct Jar(
    crate::Name,
    crate::NailFile,
    crate::build_hir_file,
    crate::NailGreenNode,
    crate::ParserDiagnostics,
    crate::build_green_node,
    crate::HirFile,
    crate::HirFileSourceMap,
    crate::Function,
    crate::Struct,
    crate::Module,
    crate::UseItem,
    crate::NameSolutionPath,
    crate::Path,
);

/// [Jar]用のDBトレイトです。
pub trait HirMasterDatabase: salsa::DbWithJar<Jar> {}

impl<DB> HirMasterDatabase for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}
