use std::fmt;

use lexer::TokenKind;
use text_size::TextRange;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    ParseError(ParseError),
    TokenError(TokenError),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::ParseError(e) => write!(f, "{e}"),
            ParserError::TokenError(e) => write!(f, "{e}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenError {
    pub(super) expected: Vec<TokenKind>,
    pub(super) actual: TokenKind,
    pub(super) range: TextRange,
}

impl TokenError {
    pub fn expected(&self) -> &[TokenKind] {
        &self.expected
    }

    pub fn actual(&self) -> TokenKind {
        self.actual
    }

    pub fn range(&self) -> TextRange {
        self.range
    }
}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: expected ",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
        )?;

        let num_expected = self.expected.len();
        let is_first = |idx| idx == 0;
        let is_last = |idx| idx == num_expected - 1;

        for (idx, expected_kind) in self.expected.iter().enumerate() {
            if is_first(idx) {
                write!(f, "{expected_kind}")?;
            } else if is_last(idx) {
                write!(f, " or {expected_kind}")?;
            } else {
                write!(f, ", {expected_kind}")?;
            }
        }

        write!(f, ", in {}", self.actual)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub(super) expected: Vec<TokenKind>,
    pub(super) found: Option<TokenKind>,
    pub(super) range: TextRange,
}

impl ParseError {
    pub fn expected(&self) -> &[TokenKind] {
        &self.expected
    }

    pub fn found(&self) -> Option<TokenKind> {
        self.found
    }

    pub fn range(&self) -> TextRange {
        self.range
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: expected ",
            u32::from(self.range.start()),
            u32::from(self.range.end()),
        )?;

        let num_expected = self.expected.len();
        let is_first = |idx| idx == 0;
        let is_last = |idx| idx == num_expected - 1;

        for (idx, expected_kind) in self.expected.iter().enumerate() {
            if is_first(idx) {
                write!(f, "{expected_kind}")?;
            } else if is_last(idx) {
                write!(f, " or {expected_kind}")?;
            } else {
                write!(f, ", {expected_kind}")?;
            }
        }

        if let Some(found) = self.found {
            write!(f, ", but found {found}")?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range as StdRange;

    use super::*;

    fn check(
        expected: Vec<TokenKind>,
        found: Option<TokenKind>,
        range: StdRange<u32>,
        output: &str,
    ) {
        let error = ParseError {
            expected,
            found,
            range: {
                let start = range.start.into();
                let end = range.end.into();
                TextRange::new(start, end)
            },
        };

        assert_eq!(format!("{error}"), output);
    }

    #[test]
    fn one_expected_did_find() {
        check(
            vec![TokenKind::Eq],
            Some(TokenKind::Ident),
            10..20,
            "error at 10..20: expected '=', but found identifier",
        );
    }

    #[test]
    fn one_expected_did_not_find() {
        check(
            vec![TokenKind::RParen],
            None,
            5..6,
            "error at 5..6: expected ')'",
        );
    }

    #[test]
    fn multiple_expected_did_find() {
        check(
            vec![
                TokenKind::IntegerLiteral,
                TokenKind::Ident,
                TokenKind::Minus,
                TokenKind::LParen,
            ],
            Some(TokenKind::LetKw),
            100..105,
            "error at 100..105: expected integerLiteral, identifier, '-' or '(', but found 'let'",
        );
    }

    #[test]
    fn two_expected_did_find() {
        check(
            vec![TokenKind::Plus, TokenKind::Minus],
            Some(TokenKind::Eq),
            0..1,
            "error at 0..1: expected '+' or '-', but found '='",
        );
    }
}
