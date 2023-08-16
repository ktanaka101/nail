use syntax::SyntaxKind;

use crate::parser::ParserError;

/// パース結果のイベント
#[derive(Debug, PartialEq)]
pub(crate) enum Event {
    /// Nodeの開始を表す
    ///
    /// `FinishNode`と対になります。
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    /// Tokenの追加を表す
    ///
    /// TokenはNodeの子要素として表現されます。
    AddToken,
    /// Nodeの終了を表す
    ///
    /// `StartNode`と対になります。
    FinishNode,
    /// パース中のエラーを表す
    Error(ParserError),
    /// 置き換え対象であることを表す
    ///
    /// イベント追加時点でイベント種別/ノードが確定しない場合に、後から置き換えるために使用されます。
    Placeholder,
    /// 無視する要素を表す
    ///
    /// イベント一覧のindexをずらさないために、削除ではなく無視するイベントとして定義しています。
    Ignore,
}
