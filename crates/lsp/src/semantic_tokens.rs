use text_size::TextSize;
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};

use crate::line_index;

pub(crate) const SEMANTIC_TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::COMMENT,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::OPERATOR,
];

pub(crate) fn raw_semantic_token(token_type: SemanticTokenType) -> u32 {
    SEMANTIC_TOKEN_TYPES
        .iter()
        .position(|t| *t == token_type)
        .unwrap() as u32
}

pub(crate) struct SemanticTokensBuilder {
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
