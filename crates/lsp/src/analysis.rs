use lsp_types::Url;
use tower_lsp::lsp_types::{self, SemanticToken};

use ast::validation::ValidationError;

use crate::diagnostic::Diagnostic;
use crate::{line_index, semantic_tokens};

#[derive(Debug)]
pub struct Analysis {
    pub uri: Url,
    pub content: String,
    pub parsed: parser::Parse,
    pub validation_errors: Vec<ValidationError>,
    pub line_index: line_index::LineIndex,
}

impl Analysis {
    pub fn new(uri: Url, content: String) -> Self {
        let parsed = parser::parse(content.as_str());
        let line_index = line_index::LineIndex::new(content.as_str());

        let syntax = parsed.syntax();
        let validation_errors = ast::validation::validate(&syntax);

        Self {
            uri,
            content,
            parsed,
            validation_errors,
            line_index,
        }
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let parsing_errors = self
            .parsed
            .errors()
            .iter()
            .map(|e| Diagnostic::from_parser_error(e.clone()));

        let validation_errors = self
            .validation_errors
            .iter()
            .map(|e| Diagnostic::from_validation_error(e.clone()));

        parsing_errors.chain(validation_errors).collect()
    }

    pub fn semantic_tokens(&self) -> Vec<SemanticToken> {
        let node = self.parsed.syntax();
        semantic_tokens::traverse(&node, &self.line_index)
    }
}
