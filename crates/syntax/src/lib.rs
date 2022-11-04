use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use lexer::TokenKind;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum NailLanguage {}

impl rowan::Language for NailLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_u16().unwrap())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    // node
    Root,

    // expressions
    Literal,
    ParenExpr,
    InfixExpr,
    PrefixExpr,
    VariableRef,
    VariableDef,

    // keywords
    FnKw,
    LetKw,
    TrueKw,
    FalseKw,
    IfKw,
    ElseKw,
    ReturnKw,

    // identifier
    Ident,

    // literals
    IntegerLiteral,
    StringLiteral,
    CharLiteral,

    // symbols
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
    Equals,
    LAngle,
    RAngle,

    // delimiters
    Comma,
    Colon,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LCurly,
    RCurly,
    VerticalBar,

    // trivias
    Whitespace,
    CommentSingle,

    Error,
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::FnKw => Self::FnKw,
            TokenKind::LetKw => Self::LetKw,
            TokenKind::TrueKw => Self::TrueKw,
            TokenKind::FalseKw => Self::FalseKw,
            TokenKind::IfKw => Self::IfKw,
            TokenKind::ElseKw => Self::ElseKw,
            TokenKind::ReturnKw => Self::ReturnKw,

            TokenKind::Ident => Self::Ident,

            TokenKind::IntegerLiteral => Self::IntegerLiteral,
            TokenKind::StringLiteral => Self::StringLiteral,
            TokenKind::CharLiteral => Self::CharLiteral,

            TokenKind::Plus => Self::Plus,
            TokenKind::Minus => Self::Minus,
            TokenKind::Star => Self::Star,
            TokenKind::Slash => Self::Slash,
            TokenKind::Bang => Self::Bang,
            TokenKind::Equals => Self::Equals,
            TokenKind::LAngle => Self::LAngle,
            TokenKind::RAngle => Self::RAngle,

            TokenKind::Comma => Self::Comma,
            TokenKind::Colon => Self::Colon,
            TokenKind::Semicolon => Self::Semicolon,
            TokenKind::LParen => Self::LParen,
            TokenKind::RParen => Self::RParen,
            TokenKind::LBrace => Self::LBrace,
            TokenKind::RBrace => Self::RBrace,
            TokenKind::LCurly => Self::LCurly,
            TokenKind::RCurly => Self::RCurly,
            TokenKind::VerticalBar => Self::VerticalBar,

            TokenKind::Whitespace => Self::Whitespace,
            TokenKind::CommentSingle => Self::CommentSingle,

            TokenKind::Error => Self::Error,
        }
    }
}

pub type SyntaxNode = rowan::SyntaxNode<NailLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<NailLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<NailLanguage>;
