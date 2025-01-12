//! パース元のソースコードの情報が無損失の具象構文木(CST: concrete syntax tree)を構築するためのパーサです。
//!
//! パーサは入力の文字列を受け取り、CSTを構築します。

#![warn(missing_docs)]

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

/// 入力を元にCSTを構築します。
pub fn parse(input: &str) -> Parse {
    let tokens: Vec<_> = Lexer::new(input).collect();
    let source = Source::new(&tokens);
    let parser = Parser::new(source);
    let events = parser.parse();
    let sink = Sink::new(&tokens, events);

    sink.finish()
}

/// パースした結果
pub struct Parse {
    /// パース結果のルートノード
    pub green_node: GreenNode,
    /// パース中に発生したエラー
    pub errors: Vec<ParserError>,
}

impl fmt::Debug for Parse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.debug_tree())
    }
}

impl Parse {
    /// パースした結果をデバッグ用の文字列として返します。
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

    /// パースした結果のCSTを返します。
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    /// パース中に発生したエラーを返します。
    pub fn errors(&self) -> &[ParserError] {
        &self.errors
    }
}

/// パース結果のデバッグ出力と期待結果を比較します。
#[cfg(test)]
fn check_debug_tree(input: &str, expected_tree: expect_test::Expect) {
    let parse = parse(input);
    expected_tree.assert_eq(&parse.debug_tree());
}

/// パース結果のデバッグ出力と期待結果を比較します。
///
/// ブロック内コンテキストで入力はパースされます。
#[cfg(test)]
fn check_debug_tree_in_block(input: &str, expected_tree: expect_test::Expect) {
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
        check_debug_tree(
            "",
            expect![[r#"
                SourceFile@0..0
            "#]],
        );
    }

    #[test]
    fn parse_whitespace() {
        check_debug_tree(
            "   ",
            expect![[r#"
                SourceFile@0..3
                  Whitespace@0..3 "   "
            "#]],
        );
    }

    #[test]
    fn parse_comment() {
        check_debug_tree(
            "// hello!",
            expect![[r#"
                SourceFile@0..9
                  CommentSingle@0..9 "// hello!"
            "#]],
        );
    }

    #[test]
    fn parse_binary_expression_when_single_slash() {
        check_debug_tree(
            "fn x() { / hello }",
            expect![[r#"
                SourceFile@0..18
                  FunctionDef@0..18
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..4 "x"
                    ParamList@4..6
                      LParen@4..5 "("
                      RParen@5..6 ")"
                    Whitespace@6..7 " "
                    BlockExpr@7..18
                      LBrace@7..8 "{"
                      Whitespace@8..9 " "
                      ExprStmt@9..10
                        Error@9..10
                          Slash@9..10 "/"
                      Whitespace@10..11 " "
                      ExprStmt@11..16
                        PathExpr@11..16
                          Path@11..16
                            PathSegment@11..16
                              Ident@11..16 "hello"
                      Whitespace@16..17 " "
                      RBrace@17..18 "}"
                error at 9..10: expected '}', 'let', 'fn', 'struct', 'mod', integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '!', '[', '(', '{', 'if', 'return', 'loop', 'continue', 'break' or 'while', but found '/'
            "#]],
        );
    }

    #[test]
    fn parse_binary_expression_interspersed_with_comments() {
        check_debug_tree(
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
                  FunctionDef@1..56
                    FnKw@1..3 "fn"
                    Whitespace@3..4 " "
                    Ident@4..5 "x"
                    ParamList@5..7
                      LParen@5..6 "("
                      RParen@6..7 ")"
                    Whitespace@7..8 " "
                    BlockExpr@8..56
                      LBrace@8..9 "{"
                      Whitespace@9..14 "\n    "
                      ExprStmt@14..43
                        BinaryExpr@14..43
                          BinaryExpr@14..23
                            Literal@14..15
                              Integer@14..15 "1"
                            Whitespace@15..20 "\n    "
                            Plus@20..21 "+"
                            Whitespace@21..22 " "
                            Literal@22..23
                              Integer@22..23 "1"
                          Whitespace@23..24 " "
                          CommentSingle@24..34 "// Add one"
                          Whitespace@34..39 "\n    "
                          Plus@39..40 "+"
                          Whitespace@40..41 " "
                          Literal@41..43
                            Integer@41..43 "10"
                      Whitespace@43..44 " "
                      CommentSingle@44..54 "// Add ten"
                      Whitespace@54..55 "\n"
                      RBrace@55..56 "}"
                  Whitespace@56..69 "\n            "
            "#]],
        );
    }

    #[test]
    fn parse_tail_trivia() {
        check_debug_tree(
            r#"
fn x() {
    1

}

fn y() {
    2

}

            "#,
            expect![[r#"
                SourceFile@0..51
                  Whitespace@0..1 "\n"
                  FunctionDef@1..18
                    FnKw@1..3 "fn"
                    Whitespace@3..4 " "
                    Ident@4..5 "x"
                    ParamList@5..7
                      LParen@5..6 "("
                      RParen@6..7 ")"
                    Whitespace@7..8 " "
                    BlockExpr@8..18
                      LBrace@8..9 "{"
                      Whitespace@9..14 "\n    "
                      ExprStmt@14..15
                        Literal@14..15
                          Integer@14..15 "1"
                      Whitespace@15..17 "\n\n"
                      RBrace@17..18 "}"
                  Whitespace@18..20 "\n\n"
                  FunctionDef@20..37
                    FnKw@20..22 "fn"
                    Whitespace@22..23 " "
                    Ident@23..24 "y"
                    ParamList@24..26
                      LParen@24..25 "("
                      RParen@25..26 ")"
                    Whitespace@26..27 " "
                    BlockExpr@27..37
                      LBrace@27..28 "{"
                      Whitespace@28..33 "\n    "
                      ExprStmt@33..34
                        Literal@33..34
                          Integer@33..34 "2"
                      Whitespace@34..36 "\n\n"
                      RBrace@36..37 "}"
                  Whitespace@37..51 "\n\n            "
            "#]],
        );
    }
}
