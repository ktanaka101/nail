use std::fmt;

use logos::{Lexer, Logos};
use text_size::TextRange;

fn lex_char(lex: &mut Lexer<TokenKind>) -> Option<bool> {
    let slice = lex.slice();
    Some(slice.ends_with('\''))
}

/// トークン種別
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
pub enum TokenKind {
    // item keywords
    /// `fn`
    #[token("fn")]
    FnKw,
    /// `mod`
    #[token("mod")]
    ModKw,
    /// `use`
    #[token("use")]
    UseKw,

    // body keywords
    /// `let`
    #[token("let")]
    LetKw,
    /// `true`
    #[token("true")]
    TrueKw,
    /// `false`
    #[token("false")]
    FalseKw,
    /// `if`
    #[token("if")]
    IfKw,
    /// `else`
    #[token("else")]
    ElseKw,
    /// `return`
    #[token("return")]
    ReturnKw,

    // identifier
    /// `[A-Za-z_][A-Za-z0-9_]*`
    #[regex("[A-Za-z_][A-Za-z0-9_]*")]
    Ident,

    // literals
    /// `[0-9]+`
    #[regex("[0-9]+")]
    IntegerLiteral,
    /// `"[^"]*"`
    #[regex(r#""[^"]*""#)]
    StringLiteral,
    /// `'[^']?'?`
    ///
    /// 終了のシングルクォートが含まれている場合、`true`となります。
    /// 終了のシングルクォーとが含まれていない場合、`false`となります。
    #[regex("'[^']?'?", lex_char)]
    CharLiteral(bool),

    // symbols
    /// `+`
    #[token("+")]
    Plus,
    /// `-`
    #[token("-")]
    Minus,
    /// `*`
    #[token("*")]
    Star,
    /// `/`
    #[token("/")]
    Slash,
    /// `!`
    #[token("!")]
    Bang,
    /// `=`
    #[token("=")]
    Eq,
    /// `==`
    #[token("==")]
    Eq2,
    /// `<`
    #[token("<")]
    LAngle,
    /// `>`
    #[token(">")]
    RAngle,

    // composite symbols
    /// `->`
    #[token("->")]
    ThinArrow,

    // delimiters
    /// `,`
    #[token(",")]
    Comma,
    /// `:`
    #[token(":")]
    Colon,
    /// `::`
    #[token("::")]
    Colon2,
    /// `;`
    #[token(";")]
    Semicolon,
    /// `(`
    #[token("(")]
    LParen,
    /// `)`
    #[token(")")]
    RParen,
    /// `[`
    #[token("[")]
    LBrace,
    /// `]`
    #[token("]")]
    RBrace,
    /// `{`
    #[token("{")]
    LCurly,
    /// `}`
    #[token("}")]
    RCurly,
    /// `|`
    #[token("|")]
    Pipe,

    // trivias
    /// `[ \n]+`
    #[regex("[ \n]+")]
    Whitespace,
    /// `//.*`
    #[regex("//.*")]
    CommentSingle,

    /// トークンとして認識できなかったもの
    #[error]
    Error,

    /// `'`
    ///
    /// バリデーションエラー時の期待するトークンにのみ使用します。
    /// 例えば、`'a`のように閉じない文字リテラルの場合、バリデーションエラーとします。
    /// その際、期待されるトークンとして`SingleQuote`が使用されます。
    SingleQuote,
}

impl TokenKind {
    /// 動作上意味のないトークンかどうかを返します。
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::Whitespace | Self::CommentSingle)
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Whitespace => "whitespace",
            Self::FnKw => "'fn'",
            Self::ModKw => "'mod'",
            Self::UseKw => "'use'",
            Self::LetKw => "'let'",
            Self::TrueKw => "'true'",
            Self::FalseKw => "'false'",
            Self::IfKw => "'if'",
            Self::ElseKw => "'else'",
            Self::ReturnKw => "'return'",
            Self::Ident => "identifier",
            Self::IntegerLiteral => "integerLiteral",
            Self::StringLiteral => "stringLiteral",
            Self::CharLiteral(_) => "charLiteral",
            Self::Plus => "'+'",
            Self::Minus => "'-'",
            Self::Star => "'*'",
            Self::Slash => "'/'",
            Self::Bang => "'!'",
            Self::Eq => "'='",
            Self::Eq2 => "'=='",
            Self::LAngle => "'<'",
            Self::RAngle => "'>'",
            Self::ThinArrow => "->",
            Self::Comma => "','",
            Self::Colon => "':'",
            Self::Colon2 => "'::'",
            Self::Semicolon => "';'",
            Self::LParen => "'('",
            Self::RParen => "')'",
            Self::LBrace => "'['",
            Self::RBrace => "']'",
            Self::LCurly => "'{'",
            Self::RCurly => "'}'",
            Self::Pipe => "'|'",
            Self::CommentSingle => "comment",
            Self::SingleQuote => "\''\'",
            Self::Error => "an unrecognized token",
        })
    }
}

/// トークン
#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    /// トークンの種類
    pub kind: TokenKind,
    /// トークンに対応する文字列
    pub text: &'a str,
    /// トークンに対応する、ファイル中のトークン位置の範囲
    pub range: TextRange,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Lexer;

    fn check(input: &str, kind: TokenKind) {
        let mut lexer = Lexer::new(input);

        let token = lexer.next().unwrap();
        assert_eq!(token.kind, kind);
        assert_eq!(token.text, input);
    }

    #[test]
    fn lex_fn_keyword() {
        check("fn", TokenKind::FnKw);
    }

    #[test]
    fn lex_mod_keyword() {
        check("mod", TokenKind::ModKw);
    }

    #[test]
    fn lex_use_keyword() {
        check("use", TokenKind::UseKw);
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
    fn lex_char_identifier() {
        check("a", TokenKind::Ident);
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
    fn lex_string_literal_with_new_line() {
        check("\"  aa  \n  bb  \n  cc\"", TokenKind::StringLiteral);
    }

    #[test]
    fn lex_char_literal() {
        check("'a'", TokenKind::CharLiteral(true));
    }

    #[test]
    fn lex_char_literal_empty() {
        check("''", TokenKind::CharLiteral(true));
    }

    #[test]
    fn lex_char_literal_with_new_line() {
        check("'\n'", TokenKind::CharLiteral(true));
    }

    #[test]
    fn lex_char_literal_missing_to_terminate() {
        check("'a", TokenKind::CharLiteral(false));
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
    fn lex_star() {
        check("*", TokenKind::Star);
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
        check("=", TokenKind::Eq);
    }

    #[test]
    fn lex_equals2() {
        check("==", TokenKind::Eq2);
    }

    #[test]
    fn lex_left_angle() {
        check("<", TokenKind::LAngle);
    }

    #[test]
    fn lex_right_angle() {
        check(">", TokenKind::RAngle);
    }

    #[test]
    fn lex_thin_arrow() {
        check("->", TokenKind::ThinArrow);
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
    fn lex_colon2() {
        check("::", TokenKind::Colon2);
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
    fn lex_pipe() {
        check("|", TokenKind::Pipe);
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
