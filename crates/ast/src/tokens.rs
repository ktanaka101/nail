use syntax::{SyntaxKind, SyntaxToken};

use crate::ast_node::AstToken;

type StdString = std::string::String;

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
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, SyntaxKind::TrueKw | SyntaxKind::FalseKw)
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
    pub fn name(&self) -> &str {
        self.syntax.text()
    }
}
