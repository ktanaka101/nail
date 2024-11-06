//! Syntax tree definitions for the Nail language.
use lexer::TokenKind;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

/// The language definition for Nail.
/// Required for rowan.
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

/// The syntax kinds for Nail.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    // ---top level node---
    /// The root node of the syntax tree.
    SourceFile,

    // ---expression nodes---
    /// `INTEGER_LITERAL` | `CHAR_LITERAL` | `STRING_LITERAL` | `true` | `false`
    Literal,
    /// `[EXPR, ...]`
    ArrayExpr,
    /// `(EXPR)`
    ParenExpr,
    /// `EXPR + EXPR`, `EXPR - EXPR`, ...
    BinaryExpr,
    /// `-EXPR`, `!EXPR`, ...
    UnaryExpr,
    /// `{ STMT* }` | `{ STMT+, EXPR }`
    BlockExpr,
    /// `EXPR(EXPR*)`
    CallExpr,
    /// `if EXPR BlockExpr else BlockExpr`
    IfExpr,
    /// `return EXPR`
    ReturnExpr,
    /// `EXPR;`
    ExprStmt,
    /// `Path`
    PathExpr,
    /// `loop BlockExpr`
    LoopExpr,
    /// `continue`
    ContinueExpr,
    /// `break`
    BreakExpr,
    /// `while EXPR BlockExpr`
    WhileExpr,
    /// `Ident RecordFieldListExpr`
    RecordExpr,
    /// `Path.NameRef`
    FieldExpr,

    // ---statement nodes---
    /// `let Ident: PathType = EXPR`
    Let,

    // ---item nodes---
    /// `fn Ident(ParamList) -> ReturnType BlockExpr`
    FunctionDef,
    /// `struct Ident { TupleFieldList | RecordFieldList }`
    ///
    /// Represents a structure definition,
    /// which can either be a tuple-like struct (e.g., `struct Ident(TupleFieldList);`) or
    /// a classic C-like struct (e.g., `struct Ident { RecordFieldList }`).
    StructDef,
    /// `mod Ident { ItemList }`
    Module,
    /// `use Path;`
    Use,

    // ---part nodes---
    /// `Param, Param, ...`
    ParamList,
    /// `IDENT: PathType`
    Param,
    /// `Arg, Arg, ...`
    ArgList,
    /// `EXPR`
    Arg,
    /// `(TupleField, TupleField, ...)`
    TupleFieldList,
    /// `PathType`
    TupleField,
    /// `{ RecordField, RecordField, ... }`
    RecordFieldList,
    /// `IDENT: PathType`
    RecordField,
    /// `{ RecordFieldExpr, RecordFieldExpr, ... }`
    RecordFieldListExpr,
    /// `IDENT: EXPR`
    RecordFieldExpr,
    /// fn foo() -> i32 { ... }
    ///             ^^^
    ///
    /// `PathType`
    ReturnType,
    /// `Item*`
    ItemList,
    /// `PathSegment::PathSegment::...`
    Path,
    /// `Ident::Ident::...`
    PathSegment,
    /// `Path`
    PathType,
    /// `Ident | Integer`
    NameRef,

    // ---item keywords---
    /// `fn`
    FnKw,
    /// `struct`
    StructKw,
    /// `mod`
    ModKw,
    /// `use`
    UseKw,

    // ---body keywords---
    /// `let`
    LetKw,
    /// `true`
    TrueKw,
    /// `false`
    FalseKw,
    /// `if`
    IfKw,
    /// `else`
    ElseKw,
    /// `return`
    ReturnKw,
    /// `loop`
    LoopKw,
    /// `while`
    WhileKw,
    /// `continue`
    ContinueKw,
    /// `break`
    BreakKw,

    // ident modifiers
    /// `mut`
    MutKw,

    // ---identifier---
    /// `IDENT`
    Ident,

    // ---literals---
    /// `INTEGER_LITERAL`, ex. 10
    Integer,
    /// `STRING_LITERAL`, ex. "hello"
    String,
    /// `CHAR_LITERAL`, ex. 'a'
    Char,

    // ---symbols---
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `!`
    Bang,
    /// `=`
    Eq,
    /// `==`
    Eq2,
    /// `!=`
    NotEq,
    /// `>`
    RAngle,
    /// `<`
    LAngle,
    /// `>=`
    GtEq,
    /// `<=`
    LtEq,

    // ---composite symbols---
    /// `->`
    ThinArrow,

    // ---delimiters---
    /// `,`
    Comma,
    /// ``
    Dot,
    /// `:`
    Colon,
    /// `::`
    Colon2,
    /// `;`
    Semicolon,
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `[`
    LBrace,
    /// `]`
    RBrace,
    /// `{`
    LCurly,
    /// `}`
    RCurly,
    /// `|`
    Pipe,

    // ---trivias---
    /// `WHITESPACE`
    Whitespace,
    /// `// COMMENT`
    CommentSingle,

    // ---error---
    /// `ERROR`
    Error,
}

impl From<TokenKind> for SyntaxKind {
    fn from(token_kind: TokenKind) -> Self {
        match token_kind {
            TokenKind::FnKw => Self::FnKw,
            TokenKind::StructKw => Self::StructKw,
            TokenKind::ModKw => Self::ModKw,
            TokenKind::UseKw => Self::UseKw,

            TokenKind::LetKw => Self::LetKw,
            TokenKind::TrueKw => Self::TrueKw,
            TokenKind::FalseKw => Self::FalseKw,
            TokenKind::IfKw => Self::IfKw,
            TokenKind::ElseKw => Self::ElseKw,
            TokenKind::ReturnKw => Self::ReturnKw,
            TokenKind::LoopKw => Self::LoopKw,
            TokenKind::WhileKw => Self::WhileKw,
            TokenKind::ContinueKw => Self::ContinueKw,
            TokenKind::BreakKw => Self::BreakKw,
            TokenKind::MutKw => Self::MutKw,

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
            TokenKind::NotEq => Self::NotEq,
            TokenKind::RAngle => Self::RAngle,
            TokenKind::LAngle => Self::LAngle,
            TokenKind::GtEq => Self::GtEq,
            TokenKind::LtEq => Self::LtEq,

            TokenKind::ThinArrow => Self::ThinArrow,

            TokenKind::Comma => Self::Comma,
            TokenKind::Dot => Self::Dot,
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

/// The syntax node type for Nail.
pub type SyntaxNode = rowan::SyntaxNode<NailLanguage>;
/// The syntax token type for Nail.
pub type SyntaxToken = rowan::SyntaxToken<NailLanguage>;
/// The syntax element type for Nail.
pub type SyntaxElement = rowan::SyntaxElement<NailLanguage>;
/// The syntax node pointer type for Nail.
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<NailLanguage>;
