use std::sync::{Arc, Mutex};

use salsa::DebugWithDb;

use crate::FileId;

/// 入力元のNailファイルです。
#[salsa::input]
pub struct NailFile {
    /// ファイルID
    pub file_id: FileId,

    /// ルートファイルか
    pub root: bool,

    /// ファイルの内容
    #[return_ref]
    pub contents: String,
}

#[salsa::tracked]
pub struct AstSourceFile {
    /** Nailファイル */
    pub file: NailFile,

    /** AST */
    #[salsa::return_ref]
    pub source: ast::SourceFile,
}

#[derive(Default)]
#[salsa::db(crate::Jar)]
pub struct SalsaDatabase {
    storage: salsa::Storage<Self>,

    /// テスト用のログ
    logs: Option<Arc<Mutex<Vec<String>>>>,
}

impl SalsaDatabase {
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

impl salsa::Database for SalsaDatabase {
    fn salsa_event(&self, event: salsa::Event) {
        if let Some(logs) = &self.logs {
            logs.lock()
                .unwrap()
                .push(format!("Event: {:?}", event.debug(self)));
        }
    }
}

// impl salsa::ParallelDatabase for Database {
//     fn snapshot(&self) -> salsa::Snapshot<Self> {
//         salsa::Snapshot::new(Database {
//             storage: self.storage.snapshot(),
//             logs: self.logs.clone(),
//         })
//     }
// }
