use lexer::TokenKind;
use syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

use super::expr;

pub(super) fn stmt(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(TokenKind::LetKw) {
        Some(variable_def(parser))
    } else if parser.at(TokenKind::FnKw) {
        Some(function_def(parser))
    } else {
        expr::expr(parser)
    }
}

fn variable_def(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::LetKw));
    let marker = parser.start();
    parser.bump();

    parser.expect(TokenKind::Ident);
    parser.expect(TokenKind::Eq);

    expr::expr(parser);

    marker.complete(parser, SyntaxKind::VariableDef)
}

fn function_def(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::FnKw));
    let marker = parser.start();
    parser.bump();

    parser.expect(TokenKind::Ident);

    // params
    {
        let marker = parser.start();

        parser.expect(TokenKind::LParen);
        if parser.at(TokenKind::Ident) {
            parser.bump();
            while parser.at(TokenKind::Comma) {
                parser.bump();
                parser.expect(TokenKind::Ident);
            }
        }
        parser.expect(TokenKind::RParen);

        marker.complete(parser, SyntaxKind::ParamList);
    }

    // block
    {
        if parser.at(TokenKind::LCurly) {
            let marker = parser.start();
            parser.bump();

            while !parser.at(TokenKind::RCurly) && !parser.at_end() {
                stmt(parser);
            }
            parser.expect(TokenKind::RCurly);

            marker.complete(parser, SyntaxKind::Block);
        }
    }

    marker.complete(parser, SyntaxKind::FunctionDef)
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_variable_definition() {
        check(
            "let foo = bar",
            expect![[r#"
                SourceFile@0..13
                  VariableDef@0..13
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..7 "foo"
                    Whitespace@7..8 " "
                    Eq@8..9 "="
                    Whitespace@9..10 " "
                    VariableRef@10..13
                      Ident@10..13 "bar"
            "#]],
        );
    }

    #[test]
    fn recover_on_let_token() {
        check(
            "let a =\nlet b = a",
            expect![[r#"
                SourceFile@0..17
                  VariableDef@0..8
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Eq@6..7 "="
                    Whitespace@7..8 "\n"
                  VariableDef@8..17
                    LetKw@8..11 "let"
                    Whitespace@11..12 " "
                    Ident@12..13 "b"
                    Whitespace@13..14 " "
                    Eq@14..15 "="
                    Whitespace@15..16 " "
                    VariableRef@16..17
                      Ident@16..17 "a"
                error at 8..11: expected integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-' or '(', but found 'let'
            "#]],
        );
    }

    #[test]
    fn recover_on_fn_token() {
        check(
            "fn a(\nfn b() {}",
            expect![[r#"
                SourceFile@0..15
                  FunctionDef@0..6
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..4 "a"
                    ParamList@4..6
                      LParen@4..5 "("
                      Whitespace@5..6 "\n"
                  FunctionDef@6..15
                    FnKw@6..8 "fn"
                    Whitespace@8..9 " "
                    Ident@9..10 "b"
                    ParamList@10..13
                      LParen@10..11 "("
                      RParen@11..12 ")"
                      Whitespace@12..13 " "
                    Block@13..15
                      LCurly@13..14 "{"
                      RCurly@14..15 "}"
                error at 6..8: expected identifier or ')', but found 'fn'
            "#]],
        );
    }

    #[test]
    fn parse_multiple_statements() {
        check(
            "let a = 1\na",
            expect![[r#"
                SourceFile@0..11
                  VariableDef@0..10
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Eq@6..7 "="
                    Whitespace@7..8 " "
                    Literal@8..10
                      IntegerLiteral@8..9 "1"
                      Whitespace@9..10 "\n"
                  VariableRef@10..11
                    Ident@10..11 "a"
            "#]],
        );
    }

    #[test]
    fn parse_function_definition() {
        check(
            "fn foo() {}",
            expect![[r#"
                SourceFile@0..11
                  FunctionDef@0..11
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..9
                      LParen@6..7 "("
                      RParen@7..8 ")"
                      Whitespace@8..9 " "
                    Block@9..11
                      LCurly@9..10 "{"
                      RCurly@10..11 "}"
            "#]],
        );
    }

    #[test]
    fn parse_function_with_params_definition() {
        check(
            "fn foo(a, b) {}",
            expect![[r#"
                SourceFile@0..15
                  FunctionDef@0..15
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..13
                      LParen@6..7 "("
                      Ident@7..8 "a"
                      Comma@8..9 ","
                      Whitespace@9..10 " "
                      Ident@10..11 "b"
                      RParen@11..12 ")"
                      Whitespace@12..13 " "
                    Block@13..15
                      LCurly@13..14 "{"
                      RCurly@14..15 "}"
            "#]],
        );
    }

    #[test]
    fn parse_function_with_block_definition() {
        check(
            "fn foo() { 10 + 20 }",
            expect![[r#"
                SourceFile@0..20
                  FunctionDef@0..20
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..9
                      LParen@6..7 "("
                      RParen@7..8 ")"
                      Whitespace@8..9 " "
                    Block@9..20
                      LCurly@9..10 "{"
                      Whitespace@10..11 " "
                      InfixExpr@11..19
                        Literal@11..14
                          IntegerLiteral@11..13 "10"
                          Whitespace@13..14 " "
                        Plus@14..15 "+"
                        Whitespace@15..16 " "
                        Literal@16..19
                          IntegerLiteral@16..18 "20"
                          Whitespace@18..19 " "
                      RCurly@19..20 "}"
            "#]],
        );
    }
}
