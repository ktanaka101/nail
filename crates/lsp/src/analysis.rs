use lsp_types::Url;
use tower_lsp::lsp_types::{self, SemanticToken};

use crate::{line_index, semantic_tokens};

/// ソースコード情報、パースエラー、バリデーションエラーを含む診断情報。
#[derive(Debug)]
pub struct Analysis {
    /// ソースコードのURI
    pub uri: Url,
    /// ソースコードのファイル情報
    pub file: hir::NailFile,
    /// AST
    pub parsed: parser::Parse,
    /// 診断情報
    pub diagnostics: Vec<lsp_types::Diagnostic>,
    /// ソースコードの行情報
    pub line_index: line_index::LineIndex,
}

impl Analysis {
    pub fn semantic_tokens(&self) -> Vec<SemanticToken> {
        let node = self.parsed.syntax();
        semantic_tokens::traverse(&node, &self.line_index)
    }
}
