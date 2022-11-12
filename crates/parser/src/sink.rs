use std::mem;

use rowan::{GreenNodeBuilder, Language};

use lexer::Token;
use syntax::NailLanguage;

use crate::{event::Event, parser::ParserError, Parse};

pub(crate) struct Sink<'l, 'input> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'l [Token<'input>],
    cursor: usize,
    events: Vec<Event>,
    errors: Vec<ParserError>,
}

impl<'l, 'input> Sink<'l, 'input> {
    pub(crate) fn new(tokens: &'l [Token<'input>], events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            tokens,
            cursor: 0,
            events,
            errors: vec![],
        }
    }

    pub(crate) fn finish(mut self) -> Parse {
        for idx in 0..self.events.len() {
            match mem::replace(&mut self.events[idx], Event::Placeholder) {
                Event::StartNode {
                    kind,
                    forward_parent,
                } => {
                    let mut kinds = vec![kind];

                    let mut idx = idx;
                    let mut forward_parent = forward_parent;

                    while let Some(fp) = forward_parent {
                        idx += fp;

                        forward_parent = if let Event::StartNode {
                            kind,
                            forward_parent,
                        } =
                            mem::replace(&mut self.events[idx], Event::Placeholder)
                        {
                            kinds.push(kind);
                            forward_parent
                        } else {
                            unreachable!()
                        };
                    }

                    for kind in kinds.into_iter().rev() {
                        self.builder.start_node(NailLanguage::kind_to_raw(kind));
                    }
                }
                Event::AddToken => self.token(),
                Event::FinishNode => self.builder.finish_node(),
                Event::Error(error) => self.errors.push(error),
                Event::Placeholder => (),
            }

            self.eat_trivia();
        }

        Parse {
            green_node: self.builder.finish(),
            events: self.events,
            errors: self.errors,
        }
    }

    fn token(&mut self) {
        let Token { kind, text, .. } = self.tokens[self.cursor];

        self.builder
            .token(NailLanguage::kind_to_raw(kind.into()), text);
        self.cursor += 1;
    }

    fn eat_trivia(&mut self) {
        while let Some(token) = self.tokens.get(self.cursor) {
            if !token.kind.is_trivia() {
                break;
            }

            self.token();
        }
    }
}
