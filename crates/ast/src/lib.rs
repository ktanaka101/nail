//! This crate provides the AST for the language. It is implemented using the `rowan` crate.

mod ast_node;
mod nodes;
mod tokens;
/// バリデーション用モジュール
/// ASTのバリデーションを行うためのモジュールです。
pub mod validation;

use std::marker::PhantomData;

pub use ast_node::*;
pub use nodes::*;
pub use rowan::{GreenNode, TextRange};
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

/// ASTノードのIDです。
///
/// 1ファイル内でユニークです。
/// IDからASTを参照し、ファイル内における位置を取得することができます。
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AstPtr<T: AstNode> {
    /// ASTノード
    pub node: SyntaxNodePtr,
    _ty: PhantomData<T>,
}
impl<T: AstNode> Clone for AstPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T: AstNode> Copy for AstPtr<T> {}
/// `AstPtr`はスレッドセーフです。
/// `AstNode`はスレッドセーフではありませんが、`AstPtr`は`AstNode`の型情報のみを参照するだけのためスレッドセーフです。
unsafe impl<T: AstNode> Send for AstPtr<T> {}
unsafe impl<T: AstNode> Sync for AstPtr<T> {}
impl<T: AstNode> AstPtr<T> {
    /// 新しいASTポインタを作成します。
    #[inline]
    pub fn new(node: &T) -> Self {
        Self {
            node: SyntaxNodePtr::new(node.syntax()),
            _ty: PhantomData,
        }
    }

    /// ASTポインタを元にASTノードを取得します。
    #[inline]
    pub fn to_ast_node(&self, syntax_node: &SyntaxNode) -> Option<T> {
        T::from_ast_ptr(self, syntax_node)
    }
}
