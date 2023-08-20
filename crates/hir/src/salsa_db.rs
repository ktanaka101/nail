use std::sync::{Arc, Mutex};

use ast::AstNode;
use salsa::DebugWithDb;

use crate::{lower_root, lower_sub_module, FileId};

/// 入力元のNailファイルです。
#[salsa::input]
pub struct NailFile {
    /// ファイルID
    pub file_id: FileId,

    /// ルートファイルか
    root: bool,

    /// ファイルの内容
    #[return_ref]
    pub contents: String,
}

/// ファイルを元にASTを構築します。
#[salsa::tracked]
pub fn parse_to_ast(db: &dyn crate::Db, nail_file: NailFile) -> AstSourceFile {
    let ast = parser::parse(nail_file.contents(db));
    let ast_source_file = ast::SourceFile::cast(ast.syntax()).unwrap();
    AstSourceFile::new(db, nail_file, ast_source_file)
}

#[salsa::tracked]
pub struct AstSourceFile {
    /** Nailファイル */
    pub file: NailFile,

    /** AST */
    #[salsa::return_ref]
    pub source: ast::SourceFile,
}


/// ASTを元に[ItemTree]を構築します。
#[salsa::tracked]
pub fn build_hir(salsa_db: &dyn crate::Db, ast_source: AstSourceFile) -> crate::LowerResult {
    if ast_source.file(salsa_db).root(salsa_db) {
        lower_root(
            salsa_db,
            ast_source.file(salsa_db).file_id(salsa_db),
            ast_source.source(salsa_db),
        )
    } else {
        lower_sub_module(
            salsa_db,
            ast_source.file(salsa_db).file_id(salsa_db),
            ast_source.source(salsa_db),
        )
    }
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
