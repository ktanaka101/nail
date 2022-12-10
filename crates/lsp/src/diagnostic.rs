use ast::validation::{ValidationError, ValidationErrorKind};
use parser::{ParseError, TokenError};
use text_size::TextRange;
use tower_lsp::lsp_types;

use crate::line_index;

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

    pub fn to_lsp_diagnostic(&self, line_index: &line_index::LineIndex) -> lsp_types::Diagnostic {
        lsp_types::Diagnostic::new_simple(self.range(line_index), self.display(line_index))
    }

    fn display(&self, line_index: &line_index::LineIndex) -> String {
        let range = line_index::PositionRange::from_text_range(self.text_range(), line_index);

        format!(
            "{} at {}:{}~{}:{} {}",
            self.title(),
            range.start().line_number(),
            range.start().col_number(),
            range.end().line_number(),
            range.end().col_number(),
            self.message()
        )
    }

    fn title(&self) -> String {
        match self {
            Self::Token(_) => "token error".to_string(),
            Self::Parsing(_) => "syntax error".to_string(),
            Self::Validation(_) => "validation error".to_string(),
        }
    }

    fn range(&self, line_index: &line_index::LineIndex) -> lsp_types::Range {
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
                    message.push_str(format!("{expected}, ").as_str());
                }

                message
            }
            Self::Parsing(err) => {
                let mut message = String::new();
                if let Some(token) = err.found() {
                    message.push_str(format!("actual: {token}").as_str());
                }
                message.push_str(" expected ");
                for expected in err.expected() {
                    message.push_str(format!("{expected}, ").as_str());
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
