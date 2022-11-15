use syntax::{SyntaxKind, SyntaxToken};

pub struct Integer {
    pub syntax: SyntaxToken,
}

impl Integer {
    pub fn cast(syntax: SyntaxToken) -> Option<Self> {
        if syntax.kind() == SyntaxKind::IntegerLiteral {
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
        if syntax.kind() == SyntaxKind::StringLiteral {
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
