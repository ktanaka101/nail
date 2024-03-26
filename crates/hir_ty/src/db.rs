use crate::inference;

/// HIR-tyの全体のデータベースです。
///
/// ここに`salsa`データを定義します。
#[salsa::jar(db = HirTyMasterDatabase)]
pub struct Jar(crate::Signature, inference::lower_signature);

/// [Jar]用のDBトレイトです。
pub trait HirTyMasterDatabase: salsa::DbWithJar<Jar> + hir::HirMasterDatabase + Sync {}

impl<DB> HirTyMasterDatabase for DB where
    DB: ?Sized + salsa::DbWithJar<Jar> + salsa::DbWithJar<hir::Jar> + Sync
{
}
