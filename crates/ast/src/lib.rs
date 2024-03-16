//! This crate provides the AST for the language. It is implemented using the `rowan` crate.

mod ast_node;
mod nodes;
mod tokens;
/// バリデーション用モジュール
/// ASTのバリデーションを行うためのモジュールです。
pub mod validation;

pub use ast_node::*;
pub use nodes::*;
pub use rowan::TextRange;
pub use syntax::SyntaxNodePtr;
use syntax::{SyntaxKind, SyntaxNode};
pub use tokens::*;

/// 1ファイルを表すルートノード
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
    /// ファイルのトップレベルの[Item]を返します。
    pub fn items(&self) -> impl Iterator<Item = nodes::Item> {
        ast_node::children_nodes(self)
    }
}
