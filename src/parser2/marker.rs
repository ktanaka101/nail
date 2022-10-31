use drop_bomb::DropBomb;

use super::event::Event;
use super::Parser;
use crate::lexer2::SyntaxKind;

pub(super) struct Marker {
    pos: usize,
    bomb_when_uncompleted: DropBomb,
}

impl Marker {
    pub(super) fn new(pos: usize) -> Self {
        Self {
            pos,
            bomb_when_uncompleted: DropBomb::new("Markers need to be completed"),
        }
    }

    pub(super) fn complete(mut self, parser: &mut Parser, kind: SyntaxKind) {
        self.bomb_when_uncompleted.defuse();

        let event_at_pos = &mut parser.events[self.pos];
        assert_eq!(*event_at_pos, Event::Placeholder);

        *event_at_pos = Event::StartNode { kind };

        parser.events.push(Event::FinishNode);
    }
}
