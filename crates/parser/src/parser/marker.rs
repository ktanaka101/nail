//! チェックポイントを管理しやすくするためのマーカー
//! 参考: https://lunacookies.github.io/lang/15/

use drop_bomb::DropBomb;
use syntax::SyntaxKind;

use crate::{event::Event, parser::Parser};

/// パース中のイベント置き換えマーカー
///
/// 未完了のまま解放された際にpanicすることで、マーカーの置き換え漏れを防ぎます。
/// 必ず[Self::complete]か[Self::destroy]を呼び出してください。
pub(crate) struct NodeMarker {
    /// 置き換え対象のイベント位置
    pos: usize,
    /// 未完了のまま解放された際にpanicするための爆弾
    bomb_when_uncompleted: DropBomb,
}

impl NodeMarker {
    /// マーカーを作成します。
    ///
    /// この構造体は、未完了のまま解放されるとpanicを起こすため、
    /// 必ず[Self::complete]か[Self::destroy]を呼び出してください。
    pub(crate) fn new(pos: usize) -> Self {
        Self {
            pos,
            bomb_when_uncompleted: DropBomb::new("Markers need to be completed"),
        }
    }

    /// 置き換え対象の[Event::Placeholder]を`kind`のノードに置き換え、完了済マーカーを作成します。
    ///
    /// 作成された完了済マーカーは、元のマーカー(置き換えたノード)をラップする必要が出た場合に使用できます。
    /// その場合は[CompletedNodeMarker#precede]を使用してください。
    /// ラップする必要が出てくるのは、パース処理を進めることで親ノードが確定するケースがあるためです。
    pub(crate) fn complete(mut self, parser: &mut Parser, kind: SyntaxKind) -> CompletedNodeMarker {
        self.bomb_when_uncompleted.defuse();

        let event_at_pos = &mut parser.events[self.pos];
        assert_eq!(*event_at_pos, Event::Placeholder);

        *event_at_pos = Event::StartNode {
            kind,
            forward_parent: None,
        };

        parser.events.push(Event::FinishNode);

        CompletedNodeMarker { pos: self.pos }
    }

    /// 置き換え対象の[Event::Placeholder]を[Event::Ignore]に置き換え、完了済マーカーを作成します。
    pub(crate) fn destroy(mut self, parser: &mut Parser) -> CompletedNodeMarker {
        self.bomb_when_uncompleted.defuse();

        let event_at_pos = &mut parser.events[self.pos];
        assert_eq!(*event_at_pos, Event::Placeholder);

        *event_at_pos = Event::Ignore;

        CompletedNodeMarker { pos: self.pos }
    }
}

/// 完了済マーカー
///
/// [NodeMarker#complete]によって作成されます。
/// 元の[NodeMarker]の位置を保持し、[NodeMarker#complete]によって置き換えられた。
pub(crate) struct CompletedNodeMarker {
    pos: usize,
}

impl CompletedNodeMarker {
    /// マーカーをNodeでラップします。
    ///
    /// ラップするNodeのマーカーが返ります。
    /// この関数を呼び出した時点では、Node種別は決まらないため、
    /// 戻り値のマーカーで[NodeMarker::complete]か[NodeMarker::destroy]を必ず呼び出してください。
    pub(crate) fn precede(self, parser: &mut Parser) -> NodeMarker {
        let new_marker = parser.start();

        if let Event::StartNode {
            ref mut forward_parent,
            ..
        } = parser.events[self.pos]
        {
            *forward_parent = Some(new_marker.pos - self.pos);
        } else {
            unreachable!();
        }

        new_marker
    }
}
