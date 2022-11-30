mod event;
mod grammar;
mod parser;
mod sink;
mod source;

use std::fmt;

use lexer::Lexer;
use rowan::GreenNode;
use sink::Sink;
use source::Source;
use syntax::SyntaxNode;

use crate::parser::Parser;
pub use crate::parser::{ParseError, ParserError, TokenError};

pub fn parse(input: &str) -> Parse {
    let tokens: Vec<_> = Lexer::new(input).collect();
    let source = Source::new(&tokens);
    let parser = Parser::new(source);
    let events = parser.parse();
    let sink = Sink::new(&tokens, events);

    sink.finish()
}

pub struct Parse {
    green_node: GreenNode,
    errors: Vec<ParserError>,
}

impl fmt::Debug for Parse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.debug_tree())
    }
}

impl Parse {
    pub fn debug_tree(&self) -> String {
        let mut s = String::new();

        let tree = format!("{:#?}", self.syntax());

        s.push_str(&tree[0..tree.len() - 1]);
        s.push('\n');

        for error in &self.errors {
            s.push_str(&format!("{}\n", error));
        }

        s
    }

    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn errors(&self) -> &[ParserError] {
        &self.errors
    }
}

#[cfg(test)]
fn check(input: &str, expected_tree: expect_test::Expect) {
    let parse = parse(input);
    expected_tree.assert_eq(&parse.debug_tree());
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;

    #[test]
    fn parse_nothing() {
        check(
            "",
            expect![[r#"
                SourceFile@0..0
            "#]],
        );
    }

    #[test]
    fn parse_whitespace() {
        check(
            "   ",
            expect![[r#"
                SourceFile@0..3
                  Whitespace@0..3 "   "
            "#]],
        );
    }

    #[test]
    fn parse_comment() {
        check(
            "// hello!",
            expect![[r#"
                SourceFile@0..9
                  CommentSingle@0..9 "// hello!"
            "#]],
        );
    }

    #[test]
    fn parse_binary_expression_when_single_slash() {
        check(
            "/ hello",
            expect![[r#"
                SourceFile@0..7
                  Error@0..2
                    Slash@0..1 "/"
                    Whitespace@1..2 " "
                  VariableRef@2..7
                    Ident@2..7 "hello"
                error at 0..1: expected 'let', 'fn', integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '(' or '{', but found '/'
            "#]],
        );
    }

    #[test]
    fn parse_binary_expression_interspersed_with_comments() {
        check(
            "
1
  + 1 // Add one
  + 10 // Add ten",
            expect![[r#"
                SourceFile@0..37
                  Whitespace@0..1 "\n"
                  BinaryExpr@1..37
                    BinaryExpr@1..22
                      Literal@1..5
                        Integer@1..2 "1"
                        Whitespace@2..5 "\n  "
                      Plus@5..6 "+"
                      Whitespace@6..7 " "
                      Literal@7..22
                        Integer@7..8 "1"
                        Whitespace@8..9 " "
                        CommentSingle@9..19 "// Add one"
                        Whitespace@19..22 "\n  "
                    Plus@22..23 "+"
                    Whitespace@23..24 " "
                    Literal@24..37
                      Integer@24..26 "10"
                      Whitespace@26..27 " "
                      CommentSingle@27..37 "// Add ten"
            "#]],
        );
    }
}
