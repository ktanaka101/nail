mod event;
mod expr;
mod marker;
mod sink;
mod source;

use rowan::GreenNode;

use crate::{
    lexer2::{Lexeme, Lexer, SyntaxKind},
    syntax::SyntaxNode,
};
use event::Event;
use expr::expr;
use marker::Marker;
use sink::Sink;
use source::Source;

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
    source: Source<'l, 'input>,
    events: Vec<Event<'l>>,
}

impl<'l, 'input> Parser<'l, 'input> {
    fn new(lexemes: &'l [Lexeme<'input>]) -> Self {
        Self {
            source: Source::new(lexemes),
            events: vec![],
        }
    }

    fn parse(mut self) -> Vec<Event<'l>> {
        let marker = self.start();
        expr(&mut self);
        marker.complete(&mut self, SyntaxKind::Root);

        self.events
    }

    fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    fn bump(&mut self) {
        let Lexeme { kind, text } = self.source.next_lexeme().unwrap();

        self.events.push(Event::AddToken {
            kind: *kind,
            text: *text,
        })
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        self.source.peek_kind()
    }

    fn at(&mut self, kind: SyntaxKind) -> bool {
        self.peek() == Some(kind)
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

    #[test]
    fn parse_whitespace() {
        check(
            "   ",
            expect![[r#"
                Root@0..3
                  Whitespace@0..3 "   ""#]],
        );
    }

    #[test]
    fn parse_comment() {
        check(
            "// hello!",
            expect![[r#"
                Root@0..9
                  CommentSingle@0..9 "// hello!""#]],
        );
    }

    #[test]
    fn parse_binary_expression_when_single_slash() {
        check("/ hello", expect!["Root@0..0"]);
    }

    #[test]
    fn parse_binary_expression_interspersed_with_comments() {
        check(
            "
1
  + 1 // Add one
  + 10 // Add ten",
            expect![[r#"
                Root@0..37
                  Whitespace@0..1 "\n"
                  BinaryExpr@1..37
                    BinaryExpr@1..22
                      Literal@1..5
                        IntegerLiteral@1..2 "1"
                        Whitespace@2..5 "\n  "
                      Plus@5..6 "+"
                      Whitespace@6..7 " "
                      Literal@7..22
                        IntegerLiteral@7..8 "1"
                        Whitespace@8..9 " "
                        CommentSingle@9..19 "// Add one"
                        Whitespace@19..22 "\n  "
                    Plus@22..23 "+"
                    Whitespace@23..24 " "
                    Literal@24..37
                      IntegerLiteral@24..26 "10"
                      Whitespace@26..27 " "
                      CommentSingle@27..37 "// Add ten""#]],
        );
    }
}
