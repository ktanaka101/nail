//! Nail用のベースとなるデータベースを定義します。
//! このデータベースは、Salsaというライブラリを使って実装されています。

/// The base database for the nail.
#[derive(Default)]
#[salsa::db(hir::Jar, hir_ty::Jar)]
pub struct SalsaDatabase {
    storage: salsa::Storage<Self>,
}

unsafe impl Send for SalsaDatabase {}
unsafe impl Sync for SalsaDatabase {}

impl salsa::Database for SalsaDatabase {
    fn salsa_event(&self, _event: salsa::Event) {}
}

impl salsa::ParallelDatabase for SalsaDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(SalsaDatabase {
            storage: self.storage.snapshot(),
        })
    }
}
