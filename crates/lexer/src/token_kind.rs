use logos::Logos;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
pub enum TokenKind {
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
    #[regex("[ \n]+")]
    Whitespace,
    #[regex("//.*")]
    CommentSingle,

    #[error]
    Error,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a str,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Lexer, Token};

    fn check(input: &str, kind: TokenKind) {
        let mut lexer = Lexer::new(input);
        assert_eq!(lexer.next(), Some(Token { kind, text: input }));
    }

    #[test]
    fn lex_fn_keyword() {
        check("fn", TokenKind::FnKw);
    }

    #[test]
    fn lex_let_keyword() {
        check("let", TokenKind::LetKw);
    }

    #[test]
    fn lex_true_keyword() {
        check("true", TokenKind::TrueKw);
    }

    #[test]
    fn lex_false_keyword() {
        check("false", TokenKind::FalseKw);
    }

    #[test]
    fn lex_if_keyword() {
        check("if", TokenKind::IfKw);
    }

    #[test]
    fn lex_else_keyword() {
        check("else", TokenKind::ElseKw);
    }

    #[test]
    fn lex_return_keyword() {
        check("return", TokenKind::ReturnKw);
    }

    #[test]
    fn lex_alphabetic_identifier() {
        check("abcd", TokenKind::Ident);
    }

    #[test]
    fn lex_alphanumeric_identifier() {
        check("ab123cde456", TokenKind::Ident);
    }

    #[test]
    fn lex_mixed_case_identifier() {
        check("ABCdef", TokenKind::Ident);
    }

    #[test]
    fn lex_snake_case_identifier() {
        check("ABC_def", TokenKind::Ident);
    }

    #[test]
    fn lex_integer_literal() {
        check("123456", TokenKind::IntegerLiteral);
    }

    #[test]
    fn lex_string_literal() {
        check("\"abc\"", TokenKind::StringLiteral);
    }

    #[test]
    fn lex_string_literal_empty() {
        check("\"\"", TokenKind::StringLiteral);
    }

    #[test]
    fn lex_char_literal() {
        check("'a'", TokenKind::CharLiteral);
    }

    #[test]
    fn lex_char_literal_empty() {
        check("''", TokenKind::CharLiteral);
    }

    #[test]
    fn lex_plus() {
        check("+", TokenKind::Plus);
    }

    #[test]
    fn lex_minus() {
        check("-", TokenKind::Minus);
    }

    #[test]
    fn lex_astersisk() {
        check("*", TokenKind::Asterisk);
    }

    #[test]
    fn lex_slash() {
        check("/", TokenKind::Slash);
    }

    #[test]
    fn lex_bang() {
        check("!", TokenKind::Bang);
    }

    #[test]
    fn lex_equals() {
        check("=", TokenKind::Equals);
    }

    #[test]
    fn lex_lt() {
        check("<", TokenKind::Lt);
    }

    #[test]
    fn lex_gt() {
        check(">", TokenKind::Gt);
    }

    #[test]
    fn lex_comma() {
        check(",", TokenKind::Comma);
    }

    #[test]
    fn lex_colon() {
        check(":", TokenKind::Colon);
    }

    #[test]
    fn lex_semicolon() {
        check(";", TokenKind::Semicolon);
    }

    #[test]
    fn lex_left_parenthesis() {
        check("(", TokenKind::LParen);
    }

    #[test]
    fn lex_right_parenthesis() {
        check(")", TokenKind::RParen);
    }

    #[test]
    fn lex_left_brace() {
        check("[", TokenKind::LBrace);
    }

    #[test]
    fn lex_right_brace() {
        check("]", TokenKind::RBrace);
    }

    #[test]
    fn lex_left_curly() {
        check("{", TokenKind::LCurly);
    }

    #[test]
    fn lex_right_curly() {
        check("}", TokenKind::RCurly);
    }

    #[test]
    fn lex_vertical_bar() {
        check("|", TokenKind::VerticalBar);
    }

    #[test]
    fn lex_spaces() {
        check("   ", TokenKind::Whitespace);
    }

    #[test]
    fn lex_comment() {
        check("// foo", TokenKind::CommentSingle);
    }

    #[test]
    fn lex_spaces_and_newlines() {
        check("  \n ", TokenKind::Whitespace);
    }
}
