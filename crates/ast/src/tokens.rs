use syntax::{SyntaxKind, SyntaxToken};

pub struct Integer {
    pub syntax: SyntaxToken,
}

impl Integer {
    pub fn cast(syntax: SyntaxToken) -> Option<Self> {
        if syntax.kind() == SyntaxKind::Integer {
            Some(Self { syntax })
        } else {
            None
        }
    }

    pub fn value(&self) -> Option<i64> {
        self.syntax.text().parse().ok()
    }
}

pub struct String {
    pub syntax: SyntaxToken,
}

impl String {
    pub fn cast(syntax: SyntaxToken) -> Option<Self> {
        if syntax.kind() == SyntaxKind::String {
            Some(Self { syntax })
        } else {
            None
        }
    }

    pub fn value(&self) -> Option<std::string::String> {
        let text = self.syntax.text();
        let text = text.trim_matches('"');
        Some(text.to_string())
    }
}

pub struct Char {
    pub syntax: SyntaxToken,
}

impl Char {
    pub fn cast(syntax: SyntaxToken) -> Option<Self> {
        if syntax.kind() == SyntaxKind::CharLiteral {
            Some(Self { syntax })
        } else {
            None
        }
    }

    pub fn value(&self) -> Option<char> {
        let text = self.syntax.text();
        let text = text.trim_matches('\'');
        text.chars().next()
    }
}

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
