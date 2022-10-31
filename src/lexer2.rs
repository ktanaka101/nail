use logos::Logos;
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Logos, FromPrimitive, ToPrimitive,
)]
pub(crate) enum SyntaxKind {
    // node
    Root,

    // keywords
    #[token("fn")]
    FnKw,
    #[token("let")]
    LetKw,
    #[token("true")]
    TrueKw,
    #[token("false")]
    FalseKw,
    #[token("if")]
    IfKw,
    #[token("else")]
    ElseKw,
    #[token("return")]
    ReturnKw,

    // identifier
    #[regex("[A-Za-z_][A-Za-z0-9_]+")]
    Ident,

    // literals
    #[regex("[0-9]+")]
    IntegerLiteral,
    #[regex(r#""[^"]*""#)]
    StringLiteral,
    #[regex("'[^']*'")]
    CharLiteral,

    // symbols
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("!")]
    Bang,
    #[token("=")]
    Equals,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,

    // delimiters
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBrace,
    #[token("]")]
    RBrace,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,
    #[token("|")]
    VerticalBar,

    // trivias
    #[regex(" +")]
    Whitespace,

    #[error]
    Error,
}

pub(crate) struct Lexer<'a> {
    inner: logos::Lexer<'a, SyntaxKind>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            inner: SyntaxKind::lexer(input),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (SyntaxKind, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();

        Some((kind, text))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str, kind: SyntaxKind) {
        let mut lexer = Lexer::new(input);
        assert_eq!(lexer.next(), Some((kind, input)));
    }

    #[test]
    fn lex_fn_keyword() {
        check("fn", SyntaxKind::FnKw);
    }

    #[test]
    fn lex_let_keyword() {
        check("let", SyntaxKind::LetKw);
    }

    #[test]
    fn lex_true_keyword() {
        check("true", SyntaxKind::TrueKw);
    }

    #[test]
    fn lex_false_keyword() {
        check("false", SyntaxKind::FalseKw);
    }

    #[test]
    fn lex_if_keyword() {
        check("if", SyntaxKind::IfKw);
    }

    #[test]
    fn lex_else_keyword() {
        check("else", SyntaxKind::ElseKw);
    }

    #[test]
    fn lex_return_keyword() {
        check("return", SyntaxKind::ReturnKw);
    }

    #[test]
    fn lex_alphabetic_identifier() {
        check("abcd", SyntaxKind::Ident);
    }

    #[test]
    fn lex_alphanumeric_identifier() {
        check("ab123cde456", SyntaxKind::Ident);
    }

    #[test]
    fn lex_mixed_case_identifier() {
        check("ABCdef", SyntaxKind::Ident);
    }

    #[test]
    fn lex_snake_case_identifier() {
        check("ABC_def", SyntaxKind::Ident);
    }

    #[test]
    fn lex_integer_literal() {
        check("123456", SyntaxKind::IntegerLiteral);
    }

    #[test]
    fn lex_string_literal() {
        check("\"abc\"", SyntaxKind::StringLiteral);
    }

    #[test]
    fn lex_string_literal_empty() {
        check("\"\"", SyntaxKind::StringLiteral);
    }

    #[test]
    fn lex_char_literal() {
        check("'a'", SyntaxKind::CharLiteral);
    }

    #[test]
    fn lex_char_literal_empty() {
        check("''", SyntaxKind::CharLiteral);
    }

    #[test]
    fn lex_plus() {
        check("+", SyntaxKind::Plus);
    }

    #[test]
    fn lex_minus() {
        check("-", SyntaxKind::Minus);
    }

    #[test]
    fn lex_astersisk() {
        check("*", SyntaxKind::Asterisk);
    }

    #[test]
    fn lex_slash() {
        check("/", SyntaxKind::Slash);
    }

    #[test]
    fn lex_bang() {
        check("!", SyntaxKind::Bang);
    }

    #[test]
    fn lex_equals() {
        check("=", SyntaxKind::Equals);
    }

    #[test]
    fn lex_lt() {
        check("<", SyntaxKind::Lt);
    }

    #[test]
    fn lex_gt() {
        check(">", SyntaxKind::Gt);
    }

    #[test]
    fn lex_comma() {
        check(",", SyntaxKind::Comma);
    }

    #[test]
    fn lex_colon() {
        check(":", SyntaxKind::Colon);
    }

    #[test]
    fn lex_semicolon() {
        check(";", SyntaxKind::Semicolon);
    }

    #[test]
    fn lex_left_paren() {
        check("(", SyntaxKind::LParen);
    }

    #[test]
    fn lex_right_paren() {
        check(")", SyntaxKind::RParen);
    }

    #[test]
    fn lex_left_brace() {
        check("[", SyntaxKind::LBrace);
    }

    #[test]
    fn lex_right_brace() {
        check("]", SyntaxKind::RBrace);
    }

    #[test]
    fn lex_left_curly() {
        check("{", SyntaxKind::LCurly);
    }

    #[test]
    fn lex_right_curly() {
        check("}", SyntaxKind::RCurly);
    }

    #[test]
    fn lex_vertical_bar() {
        check("|", SyntaxKind::VerticalBar);
    }

    #[test]
    fn lex_spaces() {
        check("   ", SyntaxKind::Whitespace);
    }
}
