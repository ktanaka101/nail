//! Nail用のベースとなるデータベースを定義します。
//! このデータベースは、Salsaというライブラリを使って実装されています。

/// The base database for the nail.
#[derive(Default)]
#[salsa::db(hir::Jar, hir_ty::Jar)]
pub struct SalsaDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for SalsaDatabase {
    fn salsa_event(&self, _event: salsa::Event) {}
}
