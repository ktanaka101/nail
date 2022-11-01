pub(crate) mod marker;

use syntax::SyntaxKind;

use crate::event::Event;
use crate::grammar;
use crate::source::Source;
use marker::Marker;

pub(crate) struct Parser<'l, 'input> {
    source: Source<'l, 'input>,
    events: Vec<Event>,
}

impl<'l, 'input> Parser<'l, 'input> {
    pub(crate) fn new(source: Source<'l, 'input>) -> Self {
        Self {
            source,
            events: vec![],
        }
    }

    pub(crate) fn parse(mut self) -> Vec<Event> {
        grammar::root(&mut self);
        self.events
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    pub(crate) fn bump(&mut self) {
        self.source.next_token().unwrap();
        self.events.push(Event::AddToken)
    }

    pub(crate) fn peek(&mut self) -> Option<SyntaxKind> {
        self.source.peek_kind()
    }

    pub(crate) fn at(&mut self, kind: SyntaxKind) -> bool {
        self.peek() == Some(kind)
    }
}
