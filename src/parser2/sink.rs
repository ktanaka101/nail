use crate::{lexer2::Lexeme, syntax::NailLanguage};

use super::Event;

use rowan::{GreenNode, GreenNodeBuilder, Language};

pub(super) struct Sink<'l, 'input> {
    builder: GreenNodeBuilder<'static>,
    lexemes: &'l [Lexeme<'input>],
    events: Vec<Event<'l>>,
}

impl<'l, 'input> Sink<'l, 'input> {
    pub(super) fn new(lexemes: &'l [Lexeme<'input>], events: Vec<Event<'input>>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            lexemes,
            events,
        }
    }

    pub(super) fn finish(mut self) -> GreenNode {
        let mut reordered_events = self.events.clone();
        for (idx, event) in self.events.into_iter().enumerate() {
            if let Event::StartNodeAt { kind, checkpoint } = event {
                reordered_events.remove(idx);
                reordered_events.insert(checkpoint, Event::StartNode { kind });
            }
        }

        for event in reordered_events {
            match event {
                Event::StartNode { kind } => {
                    self.builder.start_node(NailLanguage::kind_to_raw(kind));
                }
                Event::StartNodeAt { .. } => unreachable!(),
                Event::AddToken { kind, text } => {
                    self.builder.token(NailLanguage::kind_to_raw(kind), text);
                }
                Event::FinishNode => {
                    self.builder.finish_node();
                }
            }
        }

        self.builder.finish()
    }
}
