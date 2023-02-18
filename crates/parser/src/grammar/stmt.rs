use lexer::TokenKind;
use syntax::SyntaxKind;

use super::{expr, toplevel};
use crate::parser::{marker::CompletedMarker, Parser, BLOCK_RECOVERY_SET};

pub(super) fn parse_stmt_on_block(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(TokenKind::LetKw) {
        Some(parse_variable_def(parser))
    } else if parser.at(TokenKind::FnKw) {
        Some(parse_function_def(parser, &BLOCK_RECOVERY_SET))
    } else if parser.at(TokenKind::ModKw) {
        Some(toplevel::parse_module(parser, &BLOCK_RECOVERY_SET))
    } else {
        parse_expr_stmt(parser)
    }
}

fn parse_variable_def(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::LetKw));
    let marker = parser.start();
    parser.bump();

    parser.expect_with_block_recovery_set(TokenKind::Ident, &[TokenKind::Eq]);
    parser.expect_on_block(TokenKind::Eq);

    expr::parse_expr(parser);

    if parser.at(TokenKind::Semicolon) {
        parser.bump();
    }

    marker.complete(parser, SyntaxKind::VariableDef)
}

pub(super) fn parse_function_def(
    parser: &mut Parser,
    recovery_set: &[TokenKind],
) -> CompletedMarker {
    assert!(parser.at(TokenKind::FnKw));

    let marker = parser.start();
    parser.bump();

    parser.expect_with_recovery_set_no_default(
        TokenKind::Ident,
        &[
            recovery_set,
            &[TokenKind::LParen, TokenKind::ThinArrow, TokenKind::LCurly],
        ]
        .concat(),
    );

    if parser.at(TokenKind::LParen) {
        parse_params(
            parser,
            &[recovery_set, &[TokenKind::ThinArrow, TokenKind::LCurly]].concat(),
        );
    }

    if parser.at(TokenKind::ThinArrow) {
        parse_return_type(parser, &[recovery_set, &[TokenKind::LCurly]].concat());
    }

    if parser.at(TokenKind::LCurly) {
        parse_block(parser);
    }

    marker.complete(parser, SyntaxKind::FunctionDef)
}

fn parse_expr_stmt(parser: &mut Parser) -> Option<CompletedMarker> {
    let marker = parser.start();

    expr::parse_expr(parser);

    if parser.at(TokenKind::Semicolon) {
        parser.bump();
    }

    Some(marker.complete(parser, SyntaxKind::ExprStmt))
}

fn parse_params(parser: &mut Parser, recovery_set: &[TokenKind]) -> CompletedMarker {
    assert!(parser.at(TokenKind::LParen));

    let recovery_set = &[recovery_set, &[TokenKind::Comma, TokenKind::RParen]].concat();

    let marker = parser.start();
    parser.bump();

    if parser.at(TokenKind::Ident) {
        {
            let marker = parser.start();
            parser.bump();

            parser.expect_with_recovery_set_no_default(TokenKind::Colon, recovery_set);
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
                parser.expect_with_recovery_set_no_default(TokenKind::Ident, recovery_set);
                parser.expect_with_recovery_set_no_default(TokenKind::Colon, recovery_set);

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
    parser.expect_with_recovery_set_no_default(TokenKind::RParen, recovery_set);

    marker.complete(parser, SyntaxKind::ParamList)
}

fn parse_return_type(parser: &mut Parser, recovery_set: &[TokenKind]) -> CompletedMarker {
    assert!(parser.at(TokenKind::ThinArrow));

    let marker = parser.start();
    parser.bump();

    {
        let marker = parser.start();
        parser.expect_with_recovery_set_no_default(TokenKind::Ident, recovery_set);
        marker.complete(parser, SyntaxKind::Type);
    }

    marker.complete(parser, SyntaxKind::ReturnType)
}

fn parse_block(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::LCurly));

    let marker = parser.start();
    parser.bump();

    while !parser.at(TokenKind::RCurly) && !parser.at_end() {
        parse_stmt_on_block(parser);
    }
    parser.expect_on_block(TokenKind::RCurly);

    marker.complete(parser, SyntaxKind::BlockExpr)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check_in_block as check;

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
                    PathExpr@10..13
                      Path@10..13
                        PathSegment@10..13
                          Ident@10..13 "bar"
            "#]],
        );
    }

    #[test]
    fn parse_expr_stmt() {
        check(
            "10;",
            expect![[r#"
                SourceFile@0..3
                  ExprStmt@0..3
                    Literal@0..2
                      Integer@0..2 "10"
                    Semicolon@2..3 ";"
            "#]],
        );
    }

    #[test]
    fn parse_expr_stmt_missing_semicolon() {
        check(
            "10",
            expect![[r#"
                SourceFile@0..2
                  ExprStmt@0..2
                    Literal@0..2
                      Integer@0..2 "10"
            "#]],
        );
    }

    #[test]
    fn parse_variable_definition_with_semicolon() {
        check(
            "let foo = 10;",
            expect![[r#"
                SourceFile@0..13
                  VariableDef@0..13
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..7 "foo"
                    Whitespace@7..8 " "
                    Eq@8..9 "="
                    Whitespace@9..10 " "
                    Literal@10..12
                      Integer@10..12 "10"
                    Semicolon@12..13 ";"
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
                error at 8..10: expected integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '!', '(', '{', 'if' or 'return'
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
                    PathExpr@16..17
                      Path@16..17
                        PathSegment@16..17
                          Ident@16..17 "a"
                error at 8..11: expected integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '!', '(', '{', 'if' or 'return', but found 'let'
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
                    BlockExpr@13..15
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
                  ExprStmt@10..11
                    PathExpr@10..11
                      Path@10..11
                        PathSegment@10..11
                          Ident@10..11 "a"
            "#]],
        );
    }

    #[test]
    fn parse_function_definition() {
        check(
            "fn foo() -> int {}",
            expect![[r#"
                SourceFile@0..18
                  FunctionDef@0..18
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..9
                      LParen@6..7 "("
                      RParen@7..8 ")"
                      Whitespace@8..9 " "
                    ReturnType@9..16
                      ThinArrow@9..11 "->"
                      Whitespace@11..12 " "
                      Type@12..16
                        Ident@12..15 "int"
                        Whitespace@15..16 " "
                    BlockExpr@16..18
                      LCurly@16..17 "{"
                      RCurly@17..18 "}"
            "#]],
        );
    }

    #[test]
    fn parse_function_with_params_definition() {
        check(
            "fn foo(a: int, b: int) -> int {}",
            expect![[r#"
                SourceFile@0..32
                  FunctionDef@0..32
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
                    ReturnType@23..30
                      ThinArrow@23..25 "->"
                      Whitespace@25..26 " "
                      Type@26..30
                        Ident@26..29 "int"
                        Whitespace@29..30 " "
                    BlockExpr@30..32
                      LCurly@30..31 "{"
                      RCurly@31..32 "}"
            "#]],
        );
    }

    #[test]
    fn parse_function_with_block_definition() {
        check(
            "fn foo() -> int { 10 + 20 }",
            expect![[r#"
                SourceFile@0..27
                  FunctionDef@0..27
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..9
                      LParen@6..7 "("
                      RParen@7..8 ")"
                      Whitespace@8..9 " "
                    ReturnType@9..16
                      ThinArrow@9..11 "->"
                      Whitespace@11..12 " "
                      Type@12..16
                        Ident@12..15 "int"
                        Whitespace@15..16 " "
                    BlockExpr@16..27
                      LCurly@16..17 "{"
                      Whitespace@17..18 " "
                      ExprStmt@18..26
                        BinaryExpr@18..26
                          Literal@18..21
                            Integer@18..20 "10"
                            Whitespace@20..21 " "
                          Plus@21..22 "+"
                          Whitespace@22..23 " "
                          Literal@23..26
                            Integer@23..25 "20"
                            Whitespace@25..26 " "
                      RCurly@26..27 "}"
            "#]],
        );
    }

    #[test]
    fn parse_function_missing_param() {
        check(
            "fn foo(x: int, y -> int { 10 }",
            expect![[r#"
                SourceFile@0..30
                  FunctionDef@0..30
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
                    ReturnType@17..24
                      ThinArrow@17..19 "->"
                      Whitespace@19..20 " "
                      Type@20..24
                        Ident@20..23 "int"
                        Whitespace@23..24 " "
                    BlockExpr@24..30
                      LCurly@24..25 "{"
                      Whitespace@25..26 " "
                      ExprStmt@26..29
                        Literal@26..29
                          Integer@26..28 "10"
                          Whitespace@28..29 " "
                      RCurly@29..30 "}"
                error at 17..19: expected ':', but found ->
                error at 17..19: expected identifier, ',' or ')', but found ->
            "#]],
        );

        check(
            "fn foo(x: int, -> int { 10 }",
            expect![[r#"
                SourceFile@0..28
                  FunctionDef@0..28
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
                    ReturnType@15..22
                      ThinArrow@15..17 "->"
                      Whitespace@17..18 " "
                      Type@18..22
                        Ident@18..21 "int"
                        Whitespace@21..22 " "
                    BlockExpr@22..28
                      LCurly@22..23 "{"
                      Whitespace@23..24 " "
                      ExprStmt@24..27
                        Literal@24..27
                          Integer@24..26 "10"
                          Whitespace@26..27 " "
                      RCurly@27..28 "}"
                error at 15..17: expected identifier, but found ->
                error at 15..17: expected ':', but found ->
                error at 15..17: expected identifier, ',' or ')', but found ->
            "#]],
        );

        check(
            "fn foo(x: int -> int { 10 }",
            expect![[r#"
                SourceFile@0..27
                  FunctionDef@0..27
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
                    ReturnType@14..21
                      ThinArrow@14..16 "->"
                      Whitespace@16..17 " "
                      Type@17..21
                        Ident@17..20 "int"
                        Whitespace@20..21 " "
                    BlockExpr@21..27
                      LCurly@21..22 "{"
                      Whitespace@22..23 " "
                      ExprStmt@23..26
                        Literal@23..26
                          Integer@23..25 "10"
                          Whitespace@25..26 " "
                      RCurly@26..27 "}"
                error at 14..16: expected ',' or ')', but found ->
            "#]],
        );

        check(
            "fn foo(x: -> int { 10 }",
            expect![[r#"
                SourceFile@0..23
                  FunctionDef@0..23
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..10
                      LParen@6..7 "("
                      Param@7..10
                        Ident@7..8 "x"
                        Colon@8..9 ":"
                        Whitespace@9..10 " "
                    ReturnType@10..17
                      ThinArrow@10..12 "->"
                      Whitespace@12..13 " "
                      Type@13..17
                        Ident@13..16 "int"
                        Whitespace@16..17 " "
                    BlockExpr@17..23
                      LCurly@17..18 "{"
                      Whitespace@18..19 " "
                      ExprStmt@19..22
                        Literal@19..22
                          Integer@19..21 "10"
                          Whitespace@21..22 " "
                      RCurly@22..23 "}"
                error at 10..12: expected identifier, ',' or ')', but found ->
            "#]],
        );

        check(
            "fn foo(x -> int { 10 }",
            expect![[r#"
                SourceFile@0..22
                  FunctionDef@0..22
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..9
                      LParen@6..7 "("
                      Param@7..9
                        Ident@7..8 "x"
                        Whitespace@8..9 " "
                    ReturnType@9..16
                      ThinArrow@9..11 "->"
                      Whitespace@11..12 " "
                      Type@12..16
                        Ident@12..15 "int"
                        Whitespace@15..16 " "
                    BlockExpr@16..22
                      LCurly@16..17 "{"
                      Whitespace@17..18 " "
                      ExprStmt@18..21
                        Literal@18..21
                          Integer@18..20 "10"
                          Whitespace@20..21 " "
                      RCurly@21..22 "}"
                error at 9..11: expected ':', but found ->
                error at 9..11: expected identifier, ',' or ')', but found ->
            "#]],
        );

        check(
            "fn foo( -> int { 10 }",
            expect![[r#"
                SourceFile@0..21
                  FunctionDef@0..21
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..8
                      LParen@6..7 "("
                      Whitespace@7..8 " "
                    ReturnType@8..15
                      ThinArrow@8..10 "->"
                      Whitespace@10..11 " "
                      Type@11..15
                        Ident@11..14 "int"
                        Whitespace@14..15 " "
                    BlockExpr@15..21
                      LCurly@15..16 "{"
                      Whitespace@16..17 " "
                      ExprStmt@17..20
                        Literal@17..20
                          Integer@17..19 "10"
                          Whitespace@19..20 " "
                      RCurly@20..21 "}"
                error at 8..10: expected identifier or ')', but found ->
            "#]],
        );
    }

    #[test]
    fn parse_function_missing_ident() {
        check(
            "fn (a, b) -> int { 10 }",
            expect![[r#"
                SourceFile@0..23
                  FunctionDef@0..23
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    ParamList@3..10
                      LParen@3..4 "("
                      Param@4..5
                        Ident@4..5 "a"
                      Comma@5..6 ","
                      Whitespace@6..7 " "
                      Param@7..8
                        Ident@7..8 "b"
                      RParen@8..9 ")"
                      Whitespace@9..10 " "
                    ReturnType@10..17
                      ThinArrow@10..12 "->"
                      Whitespace@12..13 " "
                      Type@13..17
                        Ident@13..16 "int"
                        Whitespace@16..17 " "
                    BlockExpr@17..23
                      LCurly@17..18 "{"
                      Whitespace@18..19 " "
                      ExprStmt@19..22
                        Literal@19..22
                          Integer@19..21 "10"
                          Whitespace@21..22 " "
                      RCurly@22..23 "}"
                error at 3..4: expected identifier, but found '('
                error at 5..6: expected ':', but found ','
                error at 8..9: expected ':', but found ')'
            "#]],
        );
    }

    #[test]
    fn parse_function_missing_return_type() {
        check(
            "fn a() { 10 }",
            expect![[r#"
                SourceFile@0..13
                  FunctionDef@0..13
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..4 "a"
                    ParamList@4..7
                      LParen@4..5 "("
                      RParen@5..6 ")"
                      Whitespace@6..7 " "
                    BlockExpr@7..13
                      LCurly@7..8 "{"
                      Whitespace@8..9 " "
                      ExprStmt@9..12
                        Literal@9..12
                          Integer@9..11 "10"
                          Whitespace@11..12 " "
                      RCurly@12..13 "}"
            "#]],
        );

        check(
            "fn a() -> { 10 }",
            expect![[r#"
                SourceFile@0..16
                  FunctionDef@0..16
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..4 "a"
                    ParamList@4..7
                      LParen@4..5 "("
                      RParen@5..6 ")"
                      Whitespace@6..7 " "
                    ReturnType@7..10
                      ThinArrow@7..9 "->"
                      Whitespace@9..10 " "
                      Type@10..10
                    BlockExpr@10..16
                      LCurly@10..11 "{"
                      Whitespace@11..12 " "
                      ExprStmt@12..15
                        Literal@12..15
                          Integer@12..14 "10"
                          Whitespace@14..15 " "
                      RCurly@15..16 "}"
                error at 10..11: expected identifier, but found '{'
            "#]],
        );

        check(
            "fn a() int { 10 }",
            expect![[r#"
                SourceFile@0..17
                  FunctionDef@0..7
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..4 "a"
                    ParamList@4..7
                      LParen@4..5 "("
                      RParen@5..6 ")"
                      Whitespace@6..7 " "
                  ExprStmt@7..11
                    PathExpr@7..11
                      Path@7..11
                        PathSegment@7..11
                          Ident@7..10 "int"
                          Whitespace@10..11 " "
                  ExprStmt@11..17
                    BlockExpr@11..17
                      LCurly@11..12 "{"
                      Whitespace@12..13 " "
                      ExprStmt@13..16
                        Literal@13..16
                          Integer@13..15 "10"
                          Whitespace@15..16 " "
                      RCurly@16..17 "}"
            "#]],
        );
    }
}
