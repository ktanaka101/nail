use lexer::TokenKind;
use syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

use super::expr;

pub(super) fn parse_stmt(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(TokenKind::LetKw) {
        Some(parse_variable_def(parser))
    } else if parser.at(TokenKind::FnKw) {
        Some(parse_function_def(parser))
    } else {
        expr::parse_expr(parser)
    }
}

fn parse_variable_def(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::LetKw));
    let marker = parser.start();
    parser.bump();

    parser.expect_with_recovery_set(TokenKind::Ident, &[TokenKind::Eq]);
    parser.expect(TokenKind::Eq);

    expr::parse_expr(parser);

    marker.complete(parser, SyntaxKind::VariableDef)
}

fn parse_function_def(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::FnKw));

    let marker = parser.start();
    parser.bump();

    parser.expect_with_recovery_set(TokenKind::Ident, &[TokenKind::LParen, TokenKind::LCurly]);

    if parser.at(TokenKind::LParen) {
        parse_params(parser, &[TokenKind::LCurly]);
    }

    if parser.at(TokenKind::LCurly) {
        parse_block(parser);
    }

    marker.complete(parser, SyntaxKind::FunctionDef)
}

fn parse_params(parser: &mut Parser, recovery_set: &[TokenKind]) -> CompletedMarker {
    assert!(parser.at(TokenKind::LParen));

    let marker = parser.start();
    parser.bump();

    if parser.at(TokenKind::Ident) {
        {
            let marker = parser.start();
            parser.bump();

            parser.expect_with_recovery_set(TokenKind::Colon, recovery_set);
            if parser.at(TokenKind::Ident) {
                {
                    let marker = parser.start();
                    parser.bump();
                    marker.complete(parser, SyntaxKind::Type);
                }
            }
            marker.complete(parser, SyntaxKind::Param);
        }
        while parser.at(TokenKind::Comma) {
            parser.bump();
            {
                let marker = parser.start();
                parser.expect_with_recovery_set(TokenKind::Ident, recovery_set);
                parser.expect_with_recovery_set(TokenKind::Colon, recovery_set);

                if parser.at(TokenKind::Ident) {
                    {
                        let marker = parser.start();
                        parser.bump();
                        marker.complete(parser, SyntaxKind::Type);
                    }
                }
                marker.complete(parser, SyntaxKind::Param);
            }
        }
    }
    parser.expect_with_recovery_set(TokenKind::RParen, recovery_set);

    marker.complete(parser, SyntaxKind::ParamList)
}

fn parse_block(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::LCurly));

    let marker = parser.start();
    parser.bump();

    while !parser.at(TokenKind::RCurly) && !parser.at_end() {
        parse_stmt(parser);
    }
    parser.expect(TokenKind::RCurly);

    marker.complete(parser, SyntaxKind::Block)
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
    fn parse_varialbe_def_without_eq() {
        check(
            "let foo 10",
            expect![[r#"
                SourceFile@0..10
                  VariableDef@0..10
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..7 "foo"
                    Whitespace@7..8 " "
                    Error@8..10
                      Integer@8..10 "10"
                error at 8..10: expected '=', but found integerLiteral
                error at 8..10: expected integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '(' or '{'
            "#]],
        )
    }

    #[test]
    fn parse_varialbe_def_without_name() {
        check(
            "let = 10",
            expect![[r#"
                SourceFile@0..8
                  VariableDef@0..8
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Eq@4..5 "="
                    Whitespace@5..6 " "
                    Literal@6..8
                      Integer@6..8 "10"
                error at 4..5: expected identifier, but found '='
            "#]],
        )
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
                error at 8..11: expected integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '(' or '{', but found 'let'
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
                      Integer@8..9 "1"
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
            "fn foo(a: int, b: int) {}",
            expect![[r#"
                SourceFile@0..25
                  FunctionDef@0..25
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..23
                      LParen@6..7 "("
                      Param@7..13
                        Ident@7..8 "a"
                        Colon@8..9 ":"
                        Whitespace@9..10 " "
                        Type@10..13
                          Ident@10..13 "int"
                      Comma@13..14 ","
                      Whitespace@14..15 " "
                      Param@15..21
                        Ident@15..16 "b"
                        Colon@16..17 ":"
                        Whitespace@17..18 " "
                        Type@18..21
                          Ident@18..21 "int"
                      RParen@21..22 ")"
                      Whitespace@22..23 " "
                    Block@23..25
                      LCurly@23..24 "{"
                      RCurly@24..25 "}"
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
                      BinaryExpr@11..19
                        Literal@11..14
                          Integer@11..13 "10"
                          Whitespace@13..14 " "
                        Plus@14..15 "+"
                        Whitespace@15..16 " "
                        Literal@16..19
                          Integer@16..18 "20"
                          Whitespace@18..19 " "
                      RCurly@19..20 "}"
            "#]],
        );
    }

    #[test]
    fn parse_function_missing_param() {
        check(
            "fn foo(x: int, y { 10 }",
            expect![[r#"
                SourceFile@0..23
                  FunctionDef@0..23
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..17
                      LParen@6..7 "("
                      Param@7..13
                        Ident@7..8 "x"
                        Colon@8..9 ":"
                        Whitespace@9..10 " "
                        Type@10..13
                          Ident@10..13 "int"
                      Comma@13..14 ","
                      Whitespace@14..15 " "
                      Param@15..17
                        Ident@15..16 "y"
                        Whitespace@16..17 " "
                    Block@17..23
                      LCurly@17..18 "{"
                      Whitespace@18..19 " "
                      Literal@19..22
                        Integer@19..21 "10"
                        Whitespace@21..22 " "
                      RCurly@22..23 "}"
                error at 17..18: expected ':', but found '{'
                error at 17..18: expected identifier, ',' or ')', but found '{'
            "#]],
        );

        check(
            "fn foo(x: int, { 10 }",
            expect![[r#"
                SourceFile@0..21
                  FunctionDef@0..21
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..15
                      LParen@6..7 "("
                      Param@7..13
                        Ident@7..8 "x"
                        Colon@8..9 ":"
                        Whitespace@9..10 " "
                        Type@10..13
                          Ident@10..13 "int"
                      Comma@13..14 ","
                      Whitespace@14..15 " "
                      Param@15..15
                    Block@15..21
                      LCurly@15..16 "{"
                      Whitespace@16..17 " "
                      Literal@17..20
                        Integer@17..19 "10"
                        Whitespace@19..20 " "
                      RCurly@20..21 "}"
                error at 15..16: expected identifier, but found '{'
                error at 15..16: expected ':', but found '{'
                error at 15..16: expected identifier, ',' or ')', but found '{'
            "#]],
        );

        check(
            "fn foo(x: int { 10 }",
            expect![[r#"
                SourceFile@0..20
                  FunctionDef@0..20
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..14
                      LParen@6..7 "("
                      Param@7..14
                        Ident@7..8 "x"
                        Colon@8..9 ":"
                        Whitespace@9..10 " "
                        Type@10..14
                          Ident@10..13 "int"
                          Whitespace@13..14 " "
                    Block@14..20
                      LCurly@14..15 "{"
                      Whitespace@15..16 " "
                      Literal@16..19
                        Integer@16..18 "10"
                        Whitespace@18..19 " "
                      RCurly@19..20 "}"
                error at 14..15: expected ',' or ')', but found '{'
            "#]],
        );

        check(
            "fn foo(x: { 10 }",
            expect![[r#"
                SourceFile@0..16
                  FunctionDef@0..16
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..10
                      LParen@6..7 "("
                      Param@7..10
                        Ident@7..8 "x"
                        Colon@8..9 ":"
                        Whitespace@9..10 " "
                    Block@10..16
                      LCurly@10..11 "{"
                      Whitespace@11..12 " "
                      Literal@12..15
                        Integer@12..14 "10"
                        Whitespace@14..15 " "
                      RCurly@15..16 "}"
                error at 10..11: expected identifier, ',' or ')', but found '{'
            "#]],
        );

        check(
            "fn foo(x { 10 }",
            expect![[r#"
                SourceFile@0..15
                  FunctionDef@0..15
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..9
                      LParen@6..7 "("
                      Param@7..9
                        Ident@7..8 "x"
                        Whitespace@8..9 " "
                    Block@9..15
                      LCurly@9..10 "{"
                      Whitespace@10..11 " "
                      Literal@11..14
                        Integer@11..13 "10"
                        Whitespace@13..14 " "
                      RCurly@14..15 "}"
                error at 9..10: expected ':', but found '{'
                error at 9..10: expected identifier, ',' or ')', but found '{'
            "#]],
        );

        check(
            "fn foo( { 10 }",
            expect![[r#"
                SourceFile@0..14
                  FunctionDef@0..14
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..8
                      LParen@6..7 "("
                      Whitespace@7..8 " "
                    Block@8..14
                      LCurly@8..9 "{"
                      Whitespace@9..10 " "
                      Literal@10..13
                        Integer@10..12 "10"
                        Whitespace@12..13 " "
                      RCurly@13..14 "}"
                error at 8..9: expected identifier or ')', but found '{'
            "#]],
        );
    }

    #[test]
    fn parse_function_missing_ident() {
        check(
            "fn (a, b) { 10 }",
            expect![[r#"
                SourceFile@0..16
                  FunctionDef@0..16
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    ParamList@3..10
                      LParen@3..4 "("
                      Param@4..8
                        Ident@4..5 "a"
                        Error@5..7
                          Comma@5..6 ","
                          Whitespace@6..7 " "
                        Type@7..8
                          Ident@7..8 "b"
                      RParen@8..9 ")"
                      Whitespace@9..10 " "
                    Block@10..16
                      LCurly@10..11 "{"
                      Whitespace@11..12 " "
                      Literal@12..15
                        Integer@12..14 "10"
                        Whitespace@14..15 " "
                      RCurly@15..16 "}"
                error at 3..4: expected identifier, but found '('
                error at 5..6: expected ':', but found ','
            "#]],
        );
    }
}
