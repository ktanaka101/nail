use syntax::{SyntaxKind, SyntaxToken};

use crate::AstToken;

macro_rules! def_ast_token {
    ($kind:ident) => {
        #[derive(Clone, PartialEq, Eq, Hash)]
        pub struct $kind {
            syntax: SyntaxToken,
        }

        impl AstToken for $kind {
            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::$kind
            }

            fn cast(syntax: SyntaxToken) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self { syntax })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxToken {
                &self.syntax
            }
        }
    };
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add(Plus),
    Sub(Minus),
    Mul(Star),
    Div(Slash),
}
def_ast_token!(Plus);
def_ast_token!(Minus);
def_ast_token!(Star);
def_ast_token!(Slash);

impl AstToken for BinaryOp {
    fn can_cast(token: SyntaxKind) -> bool {
        matches!(
            token,
            SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::Star | SyntaxKind::Slash
        )
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::Plus => Some(Self::Add(Plus { syntax })),
            SyntaxKind::Minus => Some(Self::Sub(Minus { syntax })),
            SyntaxKind::Star => Some(Self::Mul(Star { syntax })),
            SyntaxKind::Slash => Some(Self::Div(Slash { syntax })),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            Self::Add(it) => it.syntax(),
            Self::Sub(it) => it.syntax(),
            Self::Mul(it) => it.syntax(),
            Self::Div(it) => it.syntax(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg(Minus),
}
impl AstToken for UnaryOp {
    fn can_cast(token: SyntaxKind) -> bool {
        matches!(token, SyntaxKind::Minus)
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::Minus => Some(Self::Neg(Minus { syntax })),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            Self::Neg(it) => it.syntax(),
        }
    }
}
