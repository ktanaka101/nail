use lexer::TokenKind;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

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
    // top level node
    SourceFile,

    // expression nodes
    Literal,
    ParenExpr,
    BinaryExpr,
    UnaryExpr,
    BlockExpr,
    CallExpr,
    IfExpr,
    ReturnExpr,
    ExprStmt,
    PathExpr,
    WhileExpr,

    // statement nodes
    VariableDef,

    // item nodes
    FunctionDef,
    Module,
    Use,

    // part nodes
    ParamList,
    Param,
    ArgList,
    Arg,
    Type,
    ReturnType,
    ItemList,
    Path,
    PathSegment,

    // item keywords
    FnKw,
    ModKw,
    UseKw,

    // body keywords
    LetKw,
    TrueKw,
    FalseKw,
    IfKw,
    ElseKw,
    ReturnKw,
    WhileKw,

    // identifier
    Ident,

    // literals
    Integer,
    String,
    Char,

    // symbols
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
    Eq,
    Eq2,
    /// <
    LAngle,
    /// >
    RAngle,

    // composite symbols
    /// ->
    ThinArrow,

    // delimiters
    Comma,
    Colon,
    Colon2,
    Semicolon,
    /// (
    LParen,
    /// )
    RParen,
    /// [
    LBrace,
    /// ]
    RBrace,
    /// {
    LCurly,
    /// }
    RCurly,
    /// |
    Pipe,

    // trivias
    Whitespace,
    CommentSingle,

    Error,
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::FnKw => Self::FnKw,
            TokenKind::ModKw => Self::ModKw,
            TokenKind::UseKw => Self::UseKw,

            TokenKind::LetKw => Self::LetKw,
            TokenKind::TrueKw => Self::TrueKw,
            TokenKind::FalseKw => Self::FalseKw,
            TokenKind::IfKw => Self::IfKw,
            TokenKind::ElseKw => Self::ElseKw,
            TokenKind::ReturnKw => Self::ReturnKw,
            TokenKind::LoopKw => todo!(),
            TokenKind::WhileKw => Self::WhileKw,

            TokenKind::Ident => Self::Ident,

            TokenKind::IntegerLiteral => Self::Integer,
            TokenKind::StringLiteral => Self::String,
            TokenKind::CharLiteral(_) => Self::Char,

            TokenKind::Plus => Self::Plus,
            TokenKind::Minus => Self::Minus,
            TokenKind::Star => Self::Star,
            TokenKind::Slash => Self::Slash,
            TokenKind::Bang => Self::Bang,
            TokenKind::Eq => Self::Eq,
            TokenKind::Eq2 => Self::Eq2,
            TokenKind::LAngle => Self::LAngle,
            TokenKind::RAngle => Self::RAngle,

            TokenKind::ThinArrow => Self::ThinArrow,

            TokenKind::Comma => Self::Comma,
            TokenKind::Colon => Self::Colon,
            TokenKind::Colon2 => Self::Colon2,
            TokenKind::Semicolon => Self::Semicolon,
            TokenKind::LParen => Self::LParen,
            TokenKind::RParen => Self::RParen,
            TokenKind::LBrace => Self::LBrace,
            TokenKind::RBrace => Self::RBrace,
            TokenKind::LCurly => Self::LCurly,
            TokenKind::RCurly => Self::RCurly,
            TokenKind::Pipe => Self::Pipe,

            TokenKind::Whitespace => Self::Whitespace,
            TokenKind::CommentSingle => Self::CommentSingle,

            TokenKind::Error => Self::Error,

            // only token validation
            TokenKind::SingleQuote => unreachable!(),
        }
    }
}

pub type SyntaxNode = rowan::SyntaxNode<NailLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<NailLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<NailLanguage>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<NailLanguage>;
