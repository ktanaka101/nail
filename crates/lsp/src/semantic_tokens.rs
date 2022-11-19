use rowan::WalkEvent;
use syntax::{SyntaxKind, SyntaxNode};
use text_size::TextSize;
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};

use crate::line_index::{self, LineIndex};

pub(crate) const SEMANTIC_TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::COMMENT,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::OPERATOR,
];

pub(crate) fn traverse(node: &SyntaxNode, line_index: &LineIndex) -> Vec<SemanticToken> {
    let mut builder = SemanticTokensBuilder::new();

    for event in node.preorder_with_tokens() {
        match event {
            WalkEvent::Enter(node) => {
                let token_type = match node.kind() {
                    SyntaxKind::LetKw => Some(SemanticTokenType::KEYWORD),
                    SyntaxKind::Ident => Some(SemanticTokenType::VARIABLE),
                    SyntaxKind::Eq => Some(SemanticTokenType::OPERATOR),
                    SyntaxKind::Integer => Some(SemanticTokenType::NUMBER),
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

fn raw_semantic_token(token_type: SemanticTokenType) -> u32 {
    SEMANTIC_TOKEN_TYPES
        .iter()
        .position(|t| *t == token_type)
        .unwrap() as u32
}

struct SemanticTokensBuilder {
    tokens: Vec<SemanticToken>,
    prev_line: TextSize,
    prev_col: TextSize,
}
impl SemanticTokensBuilder {
    pub(crate) fn new() -> Self {
        Self {
            tokens: vec![],
            prev_line: 0.into(),
            prev_col: 0.into(),
        }
    }

    pub(crate) fn push(
        &mut self,
        node: rowan::NodeOrToken<syntax::SyntaxNode, syntax::SyntaxToken>,
        token_type: SemanticTokenType,
        line_index: &line_index::LineIndex,
    ) {
        let range = node.text_range();
        let pos = line_index.line_col(range.start());
        let line = pos.line_number().0;
        let col = pos.col_number().0;
        let delta_line: u32 = (line - self.prev_line).into();
        let delta_start: u32 = if delta_line == 0 {
            (col - self.prev_col).into()
        } else {
            col.into()
        };

        let token = SemanticToken {
            delta_line,
            delta_start,
            length: range.len().into(),
            token_type: raw_semantic_token(token_type),
            token_modifiers_bitset: 0,
        };
        self.prev_line = line;
        self.prev_col = col;
        self.tokens.push(token);
    }

    pub(crate) fn finish(self) -> Vec<SemanticToken> {
        self.tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_tokens::raw_semantic_token;

    fn check(text: String, expected: Vec<SemanticToken>) {
        let parsed = parser::parse(text.as_str());
        let line_index = line_index::LineIndex::new(text.as_str());
        let node = parsed.syntax();
        let tokens = traverse(&node, &line_index);

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
