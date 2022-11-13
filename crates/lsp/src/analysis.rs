use lsp_types::Url;
use tower_lsp::lsp_types::{self, SemanticToken, SemanticTokenType};

use ast::validation::ValidationError;
use syntax::SyntaxKind;

use crate::diagnostic::Diagnostic;
use crate::line_index;
use crate::semantic_tokens::SemanticTokensBuilder;

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
        let mut builder = SemanticTokensBuilder::new();
        let line_index = &self.line_index;

        for event in self.parsed.syntax().preorder_with_tokens() {
            use rowan::WalkEvent;
            match event {
                WalkEvent::Enter(node) => {
                    let token_type = match node.kind() {
                        SyntaxKind::LetKw => Some(SemanticTokenType::KEYWORD),
                        SyntaxKind::Ident => Some(SemanticTokenType::VARIABLE),
                        SyntaxKind::Eq => Some(SemanticTokenType::OPERATOR),
                        SyntaxKind::IntegerLiteral => Some(SemanticTokenType::NUMBER),
                        SyntaxKind::CommentSingle => Some(SemanticTokenType::COMMENT),
                        _ => None,
                    };

                    if let Some(token_type) = token_type {
                        builder.push(node, token_type, line_index);
                    }
                }
                WalkEvent::Leave(_) => (),
            }
        }

        builder.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_tokens::raw_semantic_token;

    fn check(text: String, expected: Vec<SemanticToken>) {
        let url = Url::parse("https://example.net/a/b.html").unwrap();
        let analysis = Analysis::new(url, text);
        let tokens = analysis.semantic_tokens();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_get_semantic_tokens() {
        check(
            "let a = 10".to_string(),
            vec![
                SemanticToken {
                    delta_line: 0,
                    delta_start: 0,
                    length: 3,
                    token_type: raw_semantic_token(SemanticTokenType::KEYWORD),
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 4,
                    length: 1,
                    token_type: raw_semantic_token(SemanticTokenType::VARIABLE),
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 2,
                    length: 1,
                    token_type: raw_semantic_token(SemanticTokenType::OPERATOR),
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 2,
                    length: 2,
                    token_type: raw_semantic_token(SemanticTokenType::NUMBER),
                    token_modifiers_bitset: 0,
                },
            ],
        );
    }

    #[test]
    fn test_get_semantic_tokens_with_newline() {
        check(
            r#"
let a = 1
let b = 10
  100
"#
            .to_string(),
            vec![
                SemanticToken {
                    delta_line: 1,
                    delta_start: 0,
                    length: 3,
                    token_type: raw_semantic_token(SemanticTokenType::KEYWORD),
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 4,
                    length: 1,
                    token_type: raw_semantic_token(SemanticTokenType::VARIABLE),
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 2,
                    length: 1,
                    token_type: raw_semantic_token(SemanticTokenType::OPERATOR),
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 2,
                    length: 1,
                    token_type: raw_semantic_token(SemanticTokenType::NUMBER),
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 1,
                    delta_start: 0,
                    length: 3,
                    token_type: raw_semantic_token(SemanticTokenType::KEYWORD),
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 4,
                    length: 1,
                    token_type: raw_semantic_token(SemanticTokenType::VARIABLE),
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 2,
                    length: 1,
                    token_type: raw_semantic_token(SemanticTokenType::OPERATOR),
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 0,
                    delta_start: 2,
                    length: 2,
                    token_type: raw_semantic_token(SemanticTokenType::NUMBER),
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 1,
                    delta_start: 2,
                    length: 3,
                    token_type: raw_semantic_token(SemanticTokenType::NUMBER),
                    token_modifiers_bitset: 0,
                },
            ],
        );
    }
}
