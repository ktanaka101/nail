use std::fmt;

use text_size::TextRange;

use crate::Literal;
use syntax::SyntaxNode;

#[derive(Debug, PartialEq, Eq)]
pub struct ValidationError {
    kind: ValidationErrorKind,
    range: TextRange,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValidationErrorKind {
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
    if literal.parse().is_none() {
        errors.push(ValidationError {
            kind: ValidationErrorKind::IntegerLiteralTooLarge,
            range: literal.0.first_token().unwrap().text_range(),
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Range as StdRange;

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
        check("123", &[]);
    }

    #[test]
    fn validate_too_large_literal() {
        check(
            "99999999999999999999",
            &[(ValidationErrorKind::IntegerLiteralTooLarge, (0..20))],
        );
    }
}
