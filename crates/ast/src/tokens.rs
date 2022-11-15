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
