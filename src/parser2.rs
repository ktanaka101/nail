mod event;
mod expr;
mod sink;

use rowan::GreenNode;

use crate::{
    lexer2::{Lexer, SyntaxKind},
    syntax::SyntaxNode,
};
use event::Event;
use expr::expr;
use sink::Sink;

pub fn parse(input: &str) -> Parse {
    let lexemes: Vec<_> = Lexer::new(input).collect();
    let parser = Parser::new(&lexemes);
    let events = parser.parse();
    let sink = Sink::new(&lexemes, events);

    Parse {
        green_node: sink.finish(),
    }
}

pub struct Parser<'l, 'input> {
    lexemes: &'l [(SyntaxKind, &'input str)],
    cursor: usize,
    events: Vec<Event<'l>>,
}

impl<'l, 'input> Parser<'l, 'input> {
    fn new(lexemes: &'l [(SyntaxKind, &'input str)]) -> Self {
        Self {
            lexemes,
            cursor: 0,
            events: vec![],
        }
    }

    fn parse(mut self) -> Vec<Event<'l>> {
        self.start_node(SyntaxKind::Root);
        expr(&mut self);
        self.finish_node();

        self.events
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.events.push(Event::StartNode { kind });
    }

    fn start_node_at(&mut self, checkpoint: usize, kind: SyntaxKind) {
        self.events.push(Event::StartNodeAt { kind, checkpoint });
    }

    fn finish_node(&mut self) {
        self.events.push(Event::FinishNode);
    }

    fn checkpoint(&self) -> usize {
        self.events.len()
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.lexemes.get(self.cursor).map(|(kind, _)| *kind)
    }

    fn bump(&mut self) {
        let (kind, text) = self.lexemes[self.cursor];

        self.cursor += 1;
        self.events.push(Event::AddToken { kind, text })
    }
}

pub struct Parse {
    green_node: GreenNode,
}

impl Parse {
    pub fn debug_tree(&self) -> String {
        let syntax_node = SyntaxNode::new_root(self.green_node.clone());
        let formatted = format!("{:#?}", syntax_node);

        formatted[0..formatted.len() - 1].to_string()
    }
}

#[cfg(test)]
fn check(input: &str, expected_tree: expect_test::Expect) {
    let parse = parse(input);
    expected_tree.assert_eq(&parse.debug_tree());
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect;

    #[test]
    fn parse_nothing() {
        check("", expect![[r#"Root@0..0"#]]);
    }
}
