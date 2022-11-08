use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::path;

use anyhow::Result;
use ast::validation::ValidationErrorKind;
use lsp_types::TextDocumentIdentifier;
use lsp_types::TextDocumentItem;
use lsp_types::Url;
use lsp_types::VersionedTextDocumentIdentifier;
use text_size::TextRange;
use tower_lsp::lsp_types;

use ast::validation::ValidationError;
use parser::ParseError;
use parser::TokenError;

use crate::line_index;

#[derive(Debug, Default)]
pub struct Context {
    analyses: HashMap<Url, Analysis>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_file(&mut self, text_document: TextDocumentItem) -> Result<&Analysis> {
        let analysis = Analysis::new(text_document.uri, text_document.text);

        let uri = analysis.uri.clone();
        self.analyses.insert(uri.clone(), analysis);

        Ok(self.analyses.get(&uri).unwrap())
    }

    pub fn update_file(&mut self, id: &VersionedTextDocumentIdentifier) -> Result<&Analysis> {
        let path = path::Path::new(id.uri.path());
        let content = fs::read_to_string(path)?;
        let analysis = Analysis::new(id.uri.clone(), content);

        self.analyses.insert(id.uri.clone(), analysis);

        Ok(self.analyses.get(&id.uri).unwrap())
    }

    pub fn remove_file(&mut self, text_document: TextDocumentIdentifier) -> Result<()> {
        self.analyses.remove(&text_document.uri);

        Ok(())
    }

    pub fn analyses(&self) -> Vec<&Analysis> {
        self.analyses.values().collect::<Vec<_>>()
    }
}

#[derive(Debug)]
pub struct Analysis {
    pub uri: Url,
    pub content: String,
    pub parsed: parser::Parse,
    pub line_index: line_index::LineIndex,
}

impl Analysis {
    pub fn new(uri: Url, content: String) -> Self {
        let parsed = parser::parse(content.as_str());
        let line_index = line_index::LineIndex::new(content.as_str());

        Self {
            uri,
            content,
            parsed,
            line_index,
        }
    }

    pub fn validate(&self) {
        let _parsing_errors = self.parsed.errors();

        let syntax = self.parsed.syntax();
        let _validation_errors = ast::validation::validate(&syntax);

        unimplemented!()
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        unimplemented!()
    }
}

pub enum Diagnostic {
    Token(TokenError),
    Parsing(ParseError),
    Validation(ValidationError),
}

impl Diagnostic {
    pub fn from_parser_error(error: parser::ParserError) -> Self {
        match error {
            parser::ParserError::ParseError(err) => Self::Parsing(err),
            parser::ParserError::TokenError(err) => Self::Token(err),
        }
    }

    pub fn from_validation_error(error: ast::validation::ValidationError) -> Self {
        Self::Validation(error)
    }

    pub fn display(&self, line_index: &line_index::LineIndex) -> String {
        let range = line_index::PositionRange::from_text_range(self.text_range(), line_index);

        format!(
            "{} at {}:{}: {}",
            self.severity(),
            range.start().line_number(),
            range.start().col_number(),
            self.message()
        )
    }

    pub const fn severity(&self) -> Severity {
        Severity::Error
    }

    pub fn range(&self, line_index: &line_index::LineIndex) -> lsp_types::Range {
        let range = line_index::PositionRange::from_text_range(self.text_range(), line_index);

        range.into()
    }

    fn message(&self) -> String {
        match self {
            Self::Token(err) => {
                let mut message = String::new();
                message.push_str(format!("actual: {}", err.actual()).as_str());
                message.push_str(" expected ");
                for expected in err.expected() {
                    message.push_str(format!("{}, ", expected).as_str());
                }

                message
            }
            Self::Parsing(err) => {
                let mut message = String::new();
                if let Some(token) = err.found() {
                    message.push_str(format!("actual: {}", token).as_str());
                }
                message.push_str(" expected ");
                for expected in err.expected() {
                    message.push_str(format!("{}, ", expected).as_str());
                }

                message
            }
            Self::Validation(err) => match err.kind() {
                ValidationErrorKind::IntegerLiteralTooLarge => {
                    "Integer literal too large".to_string()
                }
            },
        }
    }

    fn text_range(&self) -> TextRange {
        match self {
            Self::Token(err) => err.range(),
            Self::Parsing(err) => err.range(),
            Self::Validation(err) => err.range(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error => write!(f, "error"),
        }
    }
}
