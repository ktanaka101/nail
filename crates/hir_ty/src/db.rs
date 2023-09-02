use crate::inference;

/// ここに`salsa`データを定義します。
#[salsa::jar(db = TyHirDatabase)]
pub struct Jar(crate::lower, inference::infer);

/// [Jar]用のDBトレイトです。
pub trait TyHirDatabase: salsa::DbWithJar<Jar> + hir::HirDatabase {}

impl<DB> TyHirDatabase for DB where DB: ?Sized + salsa::DbWithJar<Jar> + salsa::DbWithJar<hir::Jar> {}
