use syntax::{SyntaxKind, SyntaxToken};

macro_rules! def_ast_token {
    ($kind:ident) => {
        #[derive(Clone, PartialEq, Eq, Hash)]
        pub struct $kind {
            pub syntax: SyntaxToken,
        }

        impl $kind {
            pub fn cast(syntax: SyntaxToken) -> Option<Self> {
                match syntax.kind() {
                    SyntaxKind::$kind => Some(Self { syntax }),
                    _ => None,
                }
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
    pub fn value(&self) -> Option<std::string::String> {
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
    pub syntax: SyntaxToken,
}
impl Bool {
    pub fn cast(syntax: SyntaxToken) -> Option<Self> {
        match syntax.kind() {
            SyntaxKind::TrueKw | SyntaxKind::FalseKw => Some(Self { syntax }),
            _ => None,
        }
    }

    pub fn value(&self) -> Option<bool> {
        match self.syntax.kind() {
            SyntaxKind::TrueKw => Some(true),
            SyntaxKind::FalseKw => Some(false),
            _ => None,
        }
    }
}
