mod ast_node;
mod nodes;
mod operators;
mod tokens;
pub mod validation;

pub use ast_node::*;
pub use nodes::*;
pub use operators::*;
pub use tokens::*;

use syntax::{SyntaxKind, SyntaxNode};

#[derive(Debug)]
pub struct SourceFile(SyntaxNode);
impl AstNode for SourceFile {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::SourceFile
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        if Self::can_cast(node.kind()) {
            Some(Self(node))
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}
impl SourceFile {
    pub fn stmts(&self) -> impl Iterator<Item = nodes::Stmt> {
        self.0.children().filter_map(nodes::Stmt::cast)
    }
}
