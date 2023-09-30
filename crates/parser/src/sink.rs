use std::mem;

use lexer::Token;
use rowan::{GreenNodeBuilder, Language};
use syntax::NailLanguage;

use crate::{event::Event, parser::ParserError, Parse};

#[derive(Debug)]
enum Step {
    StartNode { kind: syntax::SyntaxKind },
    AddToken,
    FinishNode,
    Error(ParserError),
}

#[derive(Debug, Default)]
struct StepBuilder {
    steps: Vec<Step>,
}
impl StepBuilder {
    fn start_node(&mut self, kind: syntax::SyntaxKind) {
        self.steps.push(Step::StartNode { kind });
    }
    fn add_token(&mut self) {
        self.steps.push(Step::AddToken);
    }
    fn finish_node(&mut self) {
        self.steps.push(Step::FinishNode);
    }
    fn error(&mut self, error: ParserError) {
        self.steps.push(Step::Error(error));
    }
    fn finish(self) -> Vec<Step> {
        self.steps
    }
}

/// イベントシンク
pub(crate) struct Sink<'l, 'input> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'l [Token<'input>],
    cursor: usize,
    events: Vec<Event>,
    errors: Vec<ParserError>,
}

impl<'l, 'input> Sink<'l, 'input> {
    /// イベントシンクを作成します。
    pub(crate) fn new(tokens: &'l [Token<'input>], events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            tokens,
            cursor: 0,
            events,
            errors: vec![],
        }
    }

    /// CSTを生成します。
    pub(crate) fn finish(mut self) -> Parse {
        let mut step_builder = StepBuilder::default();
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
                        step_builder.start_node(kind);
                    }
                }
                Event::AddToken => step_builder.add_token(),
                Event::FinishNode => step_builder.finish_node(),
                Event::Error(error) => step_builder.error(error),
                Event::Placeholder | Event::Ignore => (),
            }
        }
        let steps = step_builder.finish();

        for step in steps {
            match step {
                Step::StartNode { kind } => {
                    self.builder.start_node(NailLanguage::kind_to_raw(kind));
                }
                Step::AddToken => {
                    self.token();
                }
                Step::FinishNode => {
                    self.builder.finish_node();
                }
                Step::Error(error) => {
                    self.errors.push(error);
                }
            }

            self.eat_trivia();
        }

        Parse {
            green_node: self.builder.finish(),
            errors: self.errors,
        }
    }

    /// 現在のトークンを木(rowan)に追加し、1つ進めます。
    fn token(&mut self) {
        let Token { kind, text, .. } = self.tokens[self.cursor];

        self.builder
            .token(NailLanguage::kind_to_raw(kind.into()), text);
        self.cursor += 1;
    }

    /// トリビアを飛ばします。
    fn eat_trivia(&mut self) {
        while let Some(token) = self.tokens.get(self.cursor) {
            if !token.kind.is_trivia() {
                break;
            }

            self.token();
        }
    }
}
