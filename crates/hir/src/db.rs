/// HIRの全体のデータベースです。
///
/// ここに`salsa`データを定義します。
#[salsa::jar(db = HirMasterDatabase)]
pub struct Jar(
    crate::Name,
    crate::NailFile,
    crate::build_hir_file,
    crate::HirFile,
    crate::HirFileSourceMap,
    crate::Function,
    crate::Module,
    crate::UseItem,
    crate::NameSolutionPath,
    crate::Path,
);

/// [Jar]用のDBトレイトです。
pub trait HirMasterDatabase: salsa::DbWithJar<Jar> + Sync {}

impl<DB> HirMasterDatabase for DB where DB: ?Sized + salsa::DbWithJar<Jar> + Sync {}
