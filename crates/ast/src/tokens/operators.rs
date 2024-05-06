use syntax::{SyntaxKind, SyntaxToken};

use super::def_ast_token;
use crate::{Ast, AstToken};

/// Binary operators.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    /// `+`
    Add(Plus),
    /// `-`
    Sub(Minus),
    /// `*`
    Mul(Star),
    /// `/`
    Div(Slash),
    /// `==`
    Equal(Eq2),
    /// `>`
    GreaterThan(RAngle),
    /// `<`
    LessThan(LAngle),
    /// `>=`
    GtEq(GtEq),
    /// `<=`
    LtEq(LtEq),
    /// `=`
    Assign(Eq),
}
def_ast_token!(Plus);
def_ast_token!(Minus);
def_ast_token!(Star);
def_ast_token!(Slash);
def_ast_token!(Eq2);
def_ast_token!(RAngle);
def_ast_token!(LAngle);
def_ast_token!(GtEq);
def_ast_token!(LtEq);
def_ast_token!(Eq);

impl Ast for BinaryOp {}
impl AstToken for BinaryOp {
    fn can_cast(token: SyntaxKind) -> bool {
        matches!(
            token,
            SyntaxKind::Plus
                | SyntaxKind::Minus
                | SyntaxKind::Star
                | SyntaxKind::Slash
                | SyntaxKind::Eq2
                | SyntaxKind::RAngle
                | SyntaxKind::LAngle
                | SyntaxKind::GtEq
                | SyntaxKind::LtEq
                | SyntaxKind::Eq
        )
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::Plus => Some(Self::Add(Plus { syntax })),
            SyntaxKind::Minus => Some(Self::Sub(Minus { syntax })),
            SyntaxKind::Star => Some(Self::Mul(Star { syntax })),
            SyntaxKind::Slash => Some(Self::Div(Slash { syntax })),
            SyntaxKind::Eq2 => Some(Self::Equal(Eq2 { syntax })),
            SyntaxKind::RAngle => Some(Self::GreaterThan(RAngle { syntax })),
            SyntaxKind::LAngle => Some(Self::LessThan(LAngle { syntax })),
            SyntaxKind::GtEq => Some(Self::GtEq(GtEq { syntax })),
            SyntaxKind::LtEq => Some(Self::LtEq(LtEq { syntax })),
            SyntaxKind::Eq => Some(Self::Assign(Eq { syntax })),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            Self::Add(it) => it.syntax(),
            Self::Sub(it) => it.syntax(),
            Self::Mul(it) => it.syntax(),
            Self::Div(it) => it.syntax(),
            Self::Equal(it) => it.syntax(),
            Self::GreaterThan(it) => it.syntax(),
            Self::LessThan(it) => it.syntax(),
            Self::GtEq(it) => it.syntax(),
            Self::LtEq(it) => it.syntax(),
            Self::Assign(it) => it.syntax(),
        }
    }
}

/// Unary operators.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// `-`
    Neg(Minus),
    /// `!`
    Not(Bang),
}
def_ast_token!(Bang);

impl Ast for UnaryOp {}
impl AstToken for UnaryOp {
    fn can_cast(token: SyntaxKind) -> bool {
        matches!(token, SyntaxKind::Minus | SyntaxKind::Bang)
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::Minus => Some(Self::Neg(Minus { syntax })),
            SyntaxKind::Bang => Some(Self::Not(Bang { syntax })),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            Self::Neg(it) => it.syntax(),
            Self::Not(it) => it.syntax(),
        }
    }
}
