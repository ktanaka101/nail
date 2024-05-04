use rowan::TextRange;
use syntax::{SyntaxKind, SyntaxNode, SyntaxToken};

/// ASTノード
pub trait Ast {}

/// ASTノード
///
/// ASTノードはASTトークンの集合であり、ASTノードはASTトークンを持つことができます。
pub trait AstNode: Ast {
    /// 指定した[SyntaxKind]にキャスト可能かどうかを返します。
    fn can_cast(kind: SyntaxKind) -> bool;

    /// [SyntaxNode]を[Self]にキャストします。
    /// キャストできない場合は[None]を返します。
    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    /// [SyntaxNode]を返します。
    fn syntax(&self) -> &SyntaxNode;

    /// 更新用に[SyntaxNode]をクローンします。
    /// この関数が返す[SyntaxNode]を更新しても元の[SyntaxNode]は変更されません。
    fn clone_for_update(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_for_update()).unwrap()
    }

    /// It references the documentation for [rowan::api::SyntaxNode::clone_subtree].
    fn clone_subtree(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_subtree()).unwrap()
    }
}

/// ASTトークン
///
/// ASTトークンはASTノードの一部であり、ASTトークンはASTトークンを持つことができません。
pub trait AstToken: Ast {
    /// 指定した[SyntaxKind]にキャスト可能かどうかを返します。
    fn can_cast(token: SyntaxKind) -> bool;

    /// 指定した[SyntaxToken]を[Self]にキャストします。
    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    /// [SyntaxToken]を返します。
    fn syntax(&self) -> &SyntaxToken;

    /// ASTトークンが保持するテキストを返します。
    fn text(&self) -> &str {
        self.syntax().text()
    }

    /// ASTトークンが保持するテキストの範囲を返します。
    ///
    /// 範囲の開始位置はファイルの開始位置を基準としています。
    /// この関数はIDEなどで、エラーの位置を表示する際に利用されます。
    fn range(&self) -> TextRange {
        self.syntax().text_range()
    }
}

/// 指定したノードから型引数<TChildNode>を子ノードから探して最初の一つ目を返します。
/// 子ノードが見つからない場合は[None]を返します。
pub fn child_node<TNode: AstNode, TChildNode: AstNode>(node: &TNode) -> Option<TChildNode> {
    node.syntax().children().find_map(TChildNode::cast)
}

/// 指定したノードから型引数<TChildNode>を子ノードから探して全て返します。
pub fn children_nodes<TNode: AstNode, TChildNode: AstNode>(
    node: &TNode,
) -> impl Iterator<Item = TChildNode> {
    node.syntax().children().filter_map(TChildNode::cast)
}

/// 指定したノードから指定した`kind`のトークンを探して最初の一つ目を返します。
pub fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
    parent
        .children_with_tokens()
        .find_map(|it| match it.into_token() {
            Some(token) if token.kind() == kind => Some(token),
            _ => None,
        })
}

/// 指定したノードから型引数<TChildToken>を子トークンから探して最初の一つ目を返します。
pub fn child_token<TNode: AstNode, TChildToken: AstToken>(node: &TNode) -> Option<TChildToken> {
    node.syntax()
        .children_with_tokens()
        .find_map(|it| match it.into_token() {
            Some(token) => TChildToken::cast(token),
            None => None,
        })
}

/// 指定したノードから型引数<TChildToken>を子トークンから探して全て返します。
pub fn children_tokens<TNode: AstNode, TChildToken: AstToken>(
    node: &TNode,
) -> impl Iterator<Item = TChildToken> {
    node.syntax()
        .children_with_tokens()
        .filter_map(|it| match it.into_token() {
            Some(token) => TChildToken::cast(token),
            None => None,
        })
}
