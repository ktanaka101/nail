use std::sync::{Arc, Mutex};

use salsa::DebugWithDb;

use crate::{item::ParamData, HirMasterDatabase, Type};

/// SalsaのDB
#[derive(Default)]
#[salsa::db(crate::Jar)]
pub struct TestingDatabase {
    storage: salsa::Storage<Self>,

    /// テスト用のログ
    logs: Option<Arc<Mutex<Vec<String>>>>,
}

impl TestingDatabase {
    #[cfg(test)]
    #[allow(dead_code)]
    pub fn enable_logging(self) -> Self {
        assert!(self.logs.is_none());
        Self {
            storage: self.storage,
            logs: Some(Default::default()),
        }
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub fn take_logs(&mut self) -> Vec<String> {
        if let Some(logs) = &self.logs {
            std::mem::take(&mut *logs.lock().unwrap())
        } else {
            panic!("logs not enabled");
        }
    }
}

impl salsa::Database for TestingDatabase {
    fn salsa_event(&self, event: salsa::Event) {
        if let Some(logs) = &self.logs {
            logs.lock()
                .unwrap()
                .push(format!("Event: {:?}", event.debug(self)));
        }
    }
}

impl salsa::ParallelDatabase for TestingDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(TestingDatabase {
            storage: self.storage.snapshot(),
            logs: self.logs.clone(),
        })
    }
}

/// HIRの[ParamData]をテスト向けにフォーマットします。
pub fn format_parameter(db: &dyn HirMasterDatabase, param_data: &ParamData) -> String {
    let name = if let Some(name) = param_data.name {
        name.text(db)
    } else {
        "<missing>"
    };
    let mutable_text = format_mutable(param_data.mutable);
    let ty = match param_data.ty {
        Type::Integer => "int",
        Type::String => "string",
        Type::Char => "char",
        Type::Boolean => "bool",
        Type::Unit => "()",
        Type::Unknown => "<unknown>",
    };
    format!("{name}: {mutable_text}{ty}")
}

pub fn format_mutable(mutable: bool) -> &'static str {
    if mutable {
        "mut "
    } else {
        ""
    }
}
