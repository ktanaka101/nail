use text_size::TextRange;

use syntax::{SyntaxKind, SyntaxToken};

type StdString = std::string::String;

pub trait AstToken: Sized {
    fn cast(syntax: SyntaxToken) -> Option<Self>;
    fn range(&self) -> TextRange;
}

macro_rules! def_ast_token {
    ($kind:ident) => {
        #[derive(Clone, PartialEq, Eq, Hash)]
        pub struct $kind {
            syntax: SyntaxToken,
        }

        impl AstToken for $kind {
            fn cast(syntax: SyntaxToken) -> Option<Self> {
                match syntax.kind() {
                    SyntaxKind::$kind => Some(Self { syntax }),
                    _ => None,
                }
            }

            fn range(&self) -> TextRange {
                self.syntax.text_range()
            }
        }
    };
}

def_ast_token!(Integer);
impl Integer {
    pub fn value(&self) -> Option<i64> {
        self.syntax.text().parse().ok()
    }
}

def_ast_token!(String);
impl String {
    pub fn value(&self) -> Option<StdString> {
        let text = self.syntax.text();
        let text = text.trim_matches('"');
        Some(text.to_string())
    }
}

def_ast_token!(Char);
impl Char {
    pub fn value(&self) -> Option<char> {
        let text = self.syntax.text();
        let text = text.trim_matches('\'');
        text.chars().next()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Bool {
    syntax: SyntaxToken,
}
impl AstToken for Bool {
    fn cast(syntax: SyntaxToken) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::TrueKw | SyntaxKind::FalseKw => Some(Self { syntax }),
            _ => None,
        }
    }

    fn range(&self) -> TextRange {
        self.syntax.text_range()
    }
}
impl Bool {
    pub fn value(&self) -> Option<bool> {
        match self.syntax.kind() {
            SyntaxKind::TrueKw => Some(true),
            SyntaxKind::FalseKw => Some(false),
            _ => None,
        }
    }
}

def_ast_token!(Ident);
impl Ident {
    pub fn name(&self) -> StdString {
        self.syntax.text().into()
    }

    pub fn ref_name(&self) -> &str {
        self.syntax.text()
    }
}

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
