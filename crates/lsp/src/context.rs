use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::path;

use anyhow::Result;
use ast::validation::ValidationError;
use lsp_types::TextDocumentContentChangeEvent;
use lsp_types::TextDocumentIdentifier;
use lsp_types::TextDocumentItem;
use lsp_types::Url;
use lsp_types::VersionedTextDocumentIdentifier;
use parser::ParseError;
use parser::TokenError;
use text_size::TextRange;

#[derive(Debug, Default)]
pub struct Context {
    analyses: HashMap<Url, Analysis>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_file(&mut self, text_document: TextDocumentItem) -> Result<()> {
        self.analyses.insert(
            text_document.uri.clone(),
            Analysis::new(text_document.uri, text_document.text),
        );

        Ok(())
    }

    pub fn update_file(
        &mut self,
        id: &VersionedTextDocumentIdentifier,
        _changes: &[TextDocumentContentChangeEvent],
    ) -> Result<()> {
        let path = path::Path::new(id.uri.path());
        let content = fs::read_to_string(path)?;
        let analysis = Analysis::new(id.uri.clone(), content);

        self.analyses.insert(id.uri.clone(), analysis);

        Ok(())
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
}

impl Analysis {
    pub fn new(uri: Url, content: String) -> Self {
        let parsed = parser::parse(content.as_str());

        Self {
            uri,
            content,
            parsed,
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

    pub fn display(&self) -> String {
        let range = self.range();

        format!("{} at {}:{}: {}", self.severity(), 0, 0, self.message())
    }

    pub const fn severity(&self) -> Severity {
        Severity::Error
    }

    pub fn range(&self) -> lsp_types::Range {
        todo!()
    }

    fn message(&self) -> String {
        match self {
            Self::Token(err) => todo!(),
            Self::Parsing(err) => todo!(),
            Self::Validation(err) => todo!(),
        };
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
