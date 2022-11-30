mod ast_node;
mod nodes;
mod tokens;
pub mod validation;

pub use ast_node::*;
pub use nodes::*;
use syntax::{SyntaxKind, SyntaxNode};
pub use tokens::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile(SyntaxNode);
impl Ast for SourceFile {}
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
        ast_node::children_nodes(self)
    }
}
