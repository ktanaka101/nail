use syntax::{SyntaxKind, SyntaxToken};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}
impl BinaryOp {
    pub fn cast(syntax: SyntaxToken) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::Plus => Some(Self::Add),
            SyntaxKind::Minus => Some(Self::Sub),
            SyntaxKind::Star => Some(Self::Mul),
            SyntaxKind::Slash => Some(Self::Div),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
}
impl UnaryOp {
    pub fn cast(syntax: SyntaxToken) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::Minus => Some(Self::Neg),
            _ => None,
        }
    }
}
