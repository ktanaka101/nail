use std::fmt;

use syntax::SyntaxNode;
use text_size::TextRange;

use crate::{
    ast_node::{AstNode, AstToken},
    nodes::{Literal, LiteralKind},
};

/// 構築されたASTに対するバリデーションエラー
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidationError {
    kind: ValidationErrorKind,
    range: TextRange,
}

impl ValidationError {
    /// エラーの種類を取得します。
    pub fn kind(&self) -> ValidationErrorKind {
        self.kind
    }

    /// エラー位置の範囲を取得します。
    pub fn range(&self) -> TextRange {
        self.range
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: {}",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
            self.kind
        )
    }
}

/// バリデーションエラーの種類
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValidationErrorKind {
    /// 整数リテラルが整数の最大値よりも大きい
    IntegerLiteralTooLarge,
}

impl fmt::Display for ValidationErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IntegerLiteralTooLarge => write!(
                f,
                "integer literal is larger than an integer's maximum value, {}",
                u64::MAX
            ),
        }
    }
}

/// 構築されたASTに対するバリデーションエラーを取得します。
pub fn validate(node: &SyntaxNode) -> Vec<ValidationError> {
    let mut errors = vec![];

    for node in node.descendants() {
        if let Some(literal) = Literal::cast(node) {
            validate_literal(literal, &mut errors);
        }
    }

    errors
}

fn validate_literal(literal: Literal, errors: &mut Vec<ValidationError>) {
    if let LiteralKind::Integer(int) = literal.kind() {
        if int.value().is_none() {
            errors.push(ValidationError {
                kind: ValidationErrorKind::IntegerLiteralTooLarge,
                range: int.range(),
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range as StdRange;

    use super::*;

    fn check(input: &str, expected_errors: &[(ValidationErrorKind, StdRange<u32>)]) {
        let parse = parser::parse(input);

        let expected_errors: Vec<_> = expected_errors
            .iter()
            .map(|(kind, range)| ValidationError {
                kind: *kind,
                range: {
                    let start = range.start.into();
                    let end = range.end.into();
                    TextRange::new(start, end)
                },
            })
            .collect();

        assert_eq!(validate(&parse.syntax()), expected_errors);
    }

    #[test]
    fn validate_ok_literal() {
        check("fn x() { 123 }", &[]);
    }

    #[test]
    fn validate_too_large_literal() {
        check(
            "fn x() { 99999999999999999999 }",
            &[(ValidationErrorKind::IntegerLiteralTooLarge, (9..29))],
        );
    }
}
