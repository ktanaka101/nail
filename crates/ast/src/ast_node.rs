use rowan::TextRange;

use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

pub trait AstNode {
    fn can_cast(kind: SyntaxKind) -> bool;

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;

    fn clone_for_update(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_for_update()).unwrap()
    }

    fn clone_subtree(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_subtree()).unwrap()
    }
}

pub trait AstToken {
    fn can_cast(token: SyntaxKind) -> bool;

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &str {
        self.syntax().text()
    }

    fn range(&self) -> TextRange {
        self.syntax().text_range()
    }
}

pub fn child_node<TNode: AstNode, TChildNode: AstNode>(node: &TNode) -> Option<TChildNode> {
    node.syntax().children().find_map(TChildNode::cast)
}

pub fn children_nodes<TNode: AstNode, TChildNode: AstNode>(
    node: &TNode,
) -> impl Iterator<Item = TChildNode> {
    node.syntax().children().filter_map(TChildNode::cast)
}

pub fn child_token<TNode: AstNode, TChildToken: AstToken>(node: &TNode) -> Option<TChildToken> {
    node.syntax()
        .children_with_tokens()
        .filter_map(SyntaxElement::into_token)
        .find_map(TChildToken::cast)
}

pub fn children_tokens<TNode: AstNode, TChildToken: AstToken>(
    node: &TNode,
) -> impl Iterator<Item = TChildToken> {
    node.syntax()
        .children_with_tokens()
        .filter_map(SyntaxElement::into_token)
        .filter_map(TChildToken::cast)
}
