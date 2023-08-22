#[derive(Default)]
#[salsa::db(hir::Jar)]
pub struct SalsaDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for SalsaDatabase {
    fn salsa_event(&self, _event: salsa::Event) {}
}
