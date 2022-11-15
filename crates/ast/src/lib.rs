mod nodes;
mod tokens;
pub mod validation;

pub use nodes::*;
pub use tokens::*;

use syntax::{SyntaxKind, SyntaxNode};

#[derive(Debug)]
pub struct SourceFile(SyntaxNode);

impl SourceFile {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::SourceFile {
            Some(Self(node))
        } else {
            None
        }
    }

    pub fn stmts(&self) -> impl Iterator<Item = nodes::Stmt> {
        self.0.children().filter_map(nodes::Stmt::cast)
    }
}

#[cfg(test)]
mod tests {}
