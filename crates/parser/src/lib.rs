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
            s.push_str(&format!("{error}\n"));
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
fn check_in_block(input: &str, expected_tree: expect_test::Expect) {
    let tokens: Vec<_> = Lexer::new(input).collect();
    let source = Source::new(&tokens);
    let parser = Parser::new(source);
    let events = parser.parse_in_block();
    let sink = Sink::new(&tokens, events);
    let parse = sink.finish();

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
            "fn x() { / hello }",
            expect![[r#"
                SourceFile@0..18
                  FunctionDef@0..18
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..4 "x"
                    ParamList@4..7
                      LParen@4..5 "("
                      RParen@5..6 ")"
                      Whitespace@6..7 " "
                    Block@7..18
                      LCurly@7..8 "{"
                      Whitespace@8..9 " "
                      ExprStmt@9..11
                        Error@9..11
                          Slash@9..10 "/"
                          Whitespace@10..11 " "
                      ExprStmt@11..17
                        VariableRef@11..17
                          Ident@11..16 "hello"
                          Whitespace@16..17 " "
                      RCurly@17..18 "}"
                error at 9..10: expected '}', 'let', 'fn', integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '(', '{', 'if' or 'return', but found '/'
            "#]],
        );
    }

    #[test]
    fn parse_binary_expression_interspersed_with_comments() {
        check(
            r#"
fn x() {
    1
    + 1 // Add one
    + 10 // Add ten
}
            "#,
            expect![[r#"
                SourceFile@0..69
                  Whitespace@0..1 "\n"
                  FunctionDef@1..69
                    FnKw@1..3 "fn"
                    Whitespace@3..4 " "
                    Ident@4..5 "x"
                    ParamList@5..8
                      LParen@5..6 "("
                      RParen@6..7 ")"
                      Whitespace@7..8 " "
                    Block@8..69
                      LCurly@8..9 "{"
                      Whitespace@9..14 "\n    "
                      ExprStmt@14..55
                        BinaryExpr@14..55
                          BinaryExpr@14..39
                            Literal@14..20
                              Integer@14..15 "1"
                              Whitespace@15..20 "\n    "
                            Plus@20..21 "+"
                            Whitespace@21..22 " "
                            Literal@22..39
                              Integer@22..23 "1"
                              Whitespace@23..24 " "
                              CommentSingle@24..34 "// Add one"
                              Whitespace@34..39 "\n    "
                          Plus@39..40 "+"
                          Whitespace@40..41 " "
                          Literal@41..55
                            Integer@41..43 "10"
                            Whitespace@43..44 " "
                            CommentSingle@44..54 "// Add ten"
                            Whitespace@54..55 "\n"
                      RCurly@55..56 "}"
                      Whitespace@56..69 "\n            "
            "#]],
        );
    }
}
