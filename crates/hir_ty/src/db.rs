use crate::inference;

/// ここに`salsa`データを定義します。
#[salsa::jar(db = Db)]
pub struct Jar(crate::lower, inference::infer);

/// [Jar]用のDBトレイトです。
pub trait Db: salsa::DbWithJar<Jar> + hir::Db {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + salsa::DbWithJar<hir::Jar> {}
