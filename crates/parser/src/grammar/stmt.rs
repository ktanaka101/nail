use lexer::TokenKind;
use syntax::SyntaxKind;

use super::{expr, toplevel};
use crate::parser::{marker::CompletedNodeMarker, Parser, BLOCK_RECOVERY_SET};

/// ブロック内のステートメントをパースする
///
/// トップレベルのステートメントのパースと区別するために定義しています。
pub(super) fn parse_stmt_on_block(parser: &mut Parser) -> Option<CompletedNodeMarker> {
    if parser.at(TokenKind::LetKw) {
        Some(parse_let(parser))
    } else if parser.at(TokenKind::FnKw) {
        Some(parse_function_def(parser, &BLOCK_RECOVERY_SET))
    } else if parser.at(TokenKind::StructKw) {
        Some(parse_struct(parser, &BLOCK_RECOVERY_SET))
    } else if parser.at(TokenKind::ModKw) {
        Some(toplevel::parse_module(parser, &BLOCK_RECOVERY_SET))
    } else {
        parse_expr_stmt(parser)
    }
}

/// ローカル変数の定義をパースする
fn parse_let(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::LetKw));
    let marker = parser.start();
    parser.bump();

    if parser.at(TokenKind::MutKw) {
        parser.bump();
    }

    parser.expect_with_block_recovery_set(TokenKind::Ident, &[TokenKind::Eq]);
    parser.expect_on_block(TokenKind::Eq);

    expr::parse_expr(parser);

    if parser.at(TokenKind::Semicolon) {
        parser.bump();
    }

    marker.complete(parser, SyntaxKind::Let)
}

/// 関数定義をパースする
///
/// 関数定義はトップレベルとブロック内でパースするため、
/// それぞれに合わせて復帰トークン種別を`recovery_set`に指定可能にしています。
pub(super) fn parse_function_def(
    parser: &mut Parser,
    recovery_set: &[TokenKind],
) -> CompletedNodeMarker {
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

/// 構造体定義をパースする
pub(super) fn parse_struct(parser: &mut Parser, recovery_set: &[TokenKind]) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::StructKw));

    let marker = parser.start();
    parser.bump();

    parser.expect_with_recovery_set_no_default(
        TokenKind::Ident,
        &[
            recovery_set,
            &[TokenKind::LParen, TokenKind::LCurly, TokenKind::Semicolon],
        ]
        .concat(),
    );

    // No body struct
    if parser.at(TokenKind::Semicolon) {
        parser.bump();
    }
    // or tuple fields struct
    else if parser.at(TokenKind::LParen) {
        parse_tuple_fields(parser, recovery_set);
        if parser.at(TokenKind::Semicolon) {
            parser.bump();
        }
    }
    // or record fields struct
    else if parser.at(TokenKind::LCurly) {
        parse_record_fields(parser, recovery_set);
    }

    marker.complete(parser, SyntaxKind::StructDef)
}

/// ステートメントをパースする
fn parse_expr_stmt(parser: &mut Parser) -> Option<CompletedNodeMarker> {
    let marker = parser.start();

    expr::parse_expr(parser);

    if parser.at(TokenKind::Semicolon) {
        parser.bump();
    }

    Some(marker.complete(parser, SyntaxKind::ExprStmt))
}

/// 関数のパラメータをパースする
///
/// 関数定義はトップレベルとブロック内でパースするため、
/// それぞれに合わせて復帰トークン種別を`recovery_set`に指定可能にしています。
fn parse_params(parser: &mut Parser, recovery_set: &[TokenKind]) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::LParen));

    let recovery_set = &[recovery_set, &[TokenKind::Comma, TokenKind::RParen]].concat();

    let marker = parser.start();
    parser.bump();

    if parser.at(TokenKind::Ident) {
        {
            let marker = parser.start();
            parser.bump();

            parser.expect_with_recovery_set_no_default(TokenKind::Colon, recovery_set);

            if parser.at(TokenKind::MutKw) {
                parser.bump();
            }

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

                if parser.at(TokenKind::MutKw) {
                    parser.bump();
                }

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

/// 関数の戻り値の型をパースする
///
/// 関数定義はトップレベルとブロック内でパースするため、
/// それぞれに合わせて復帰トークン種別を`recovery_set`に指定可能にしています。
fn parse_return_type(parser: &mut Parser, recovery_set: &[TokenKind]) -> CompletedNodeMarker {
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

/// ブロックをパースする
fn parse_block(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::LCurly));

    let marker = parser.start();
    parser.bump();

    while !parser.at(TokenKind::RCurly) && !parser.at_end() {
        parse_stmt_on_block(parser);
    }
    parser.expect_on_block(TokenKind::RCurly);

    marker.complete(parser, SyntaxKind::BlockExpr)
}

/// 構造体のレコードフィールドをパースする
///
/// 構造体定義はトップレベルとブロック内でパースするため、
/// それぞれに合わせて復帰トークン種別を`recovery_set`に指定可能にしています。
fn parse_record_fields(parser: &mut Parser, recovery_set: &[TokenKind]) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::LCurly));

    let recovery_set = &[
        recovery_set,
        &[TokenKind::Comma, TokenKind::Colon, TokenKind::RCurly],
    ]
    .concat();

    let marker = parser.start();
    parser.bump();

    if parser.at(TokenKind::Ident) || parser.at(TokenKind::Colon) || parser.at(TokenKind::Comma) {
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
            marker.complete(parser, SyntaxKind::RecordField);
        }
        while parser.at(TokenKind::Comma) {
            parser.bump();
            {
                // struct AAA { a: int, b: int, }
                //                            ^時点で停止させる
                if parser.at(TokenKind::RCurly) {
                    parser.bump();
                    return marker.complete(parser, SyntaxKind::RecordFieldList);
                }

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
                marker.complete(parser, SyntaxKind::RecordField);
            }
        }
    }
    parser.expect_with_recovery_set_no_default(TokenKind::RCurly, recovery_set);

    marker.complete(parser, SyntaxKind::RecordFieldList)
}

/// 構造体のタプルフィールドをパースする
///
/// 構造体定義はトップレベルとブロック内でパースするため、
/// それぞれに合わせて復帰トークン種別を`recovery_set`に指定可能にしています。
fn parse_tuple_fields(parser: &mut Parser, recovery_set: &[TokenKind]) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::LParen));

    let recovery_set = &[recovery_set, &[TokenKind::Comma, TokenKind::RParen]].concat();

    let marker = parser.start();
    parser.bump();

    if parser.at(TokenKind::Ident) {
        {
            let marker = parser.start();
            parser.bump();

            if parser.at(TokenKind::Ident) {
                {
                    let marker = parser.start();
                    parser.bump();
                    marker.complete(parser, SyntaxKind::Type);
                }
            }
            marker.complete(parser, SyntaxKind::TupleField);
        }
        while parser.at(TokenKind::Comma) {
            parser.bump();
            {
                // struct AAA(i32, i32,);
                //                    ^時点で停止させる(`(`は`parse_struct`側でbump)
                if parser.at(TokenKind::RParen) {
                    parser.bump();
                    return marker.complete(parser, SyntaxKind::TupleFieldList);
                }

                let marker = parser.start();
                parser.expect_with_recovery_set_no_default(TokenKind::Ident, recovery_set);

                if parser.at(TokenKind::Ident) {
                    {
                        let marker = parser.start();
                        parser.bump();
                        marker.complete(parser, SyntaxKind::Type);
                    }
                }
                marker.complete(parser, SyntaxKind::TupleField);
            }
        }
    }
    parser.expect_with_recovery_set_no_default(TokenKind::RParen, recovery_set);

    marker.complete(parser, SyntaxKind::TupleFieldList)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check_debug_tree_in_block;

    #[test]
    fn parse_let() {
        check_debug_tree_in_block(
            "let foo = bar",
            expect![[r#"
                SourceFile@0..13
                  Let@0..13
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
    fn parse_let_with_semicolon() {
        check_debug_tree_in_block(
            "let foo = 10;",
            expect![[r#"
                SourceFile@0..13
                  Let@0..13
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
    fn parse_let_mutable() {
        check_debug_tree_in_block(
            "let mut foo = 10;",
            expect![[r#"
                SourceFile@0..17
                  Let@0..17
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    MutKw@4..7 "mut"
                    Whitespace@7..8 " "
                    Ident@8..11 "foo"
                    Whitespace@11..12 " "
                    Eq@12..13 "="
                    Whitespace@13..14 " "
                    Literal@14..16
                      Integer@14..16 "10"
                    Semicolon@16..17 ";"
            "#]],
        );
    }

    #[test]
    fn parse_varialbe_def_without_eq() {
        check_debug_tree_in_block(
            "let foo 10",
            expect![[r#"
                SourceFile@0..10
                  Let@0..10
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..7 "foo"
                    Whitespace@7..8 " "
                    Error@8..10
                      Integer@8..10 "10"
                error at 8..10: expected '=', but found integerLiteral
                error at 8..10: expected integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '!', '(', '{', 'if', 'return', 'loop', 'continue', 'break' or 'while'
            "#]],
        )
    }

    #[test]
    fn parse_varialbe_def_without_name() {
        check_debug_tree_in_block(
            "let = 10",
            expect![[r#"
                SourceFile@0..8
                  Let@0..8
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Eq@4..5 "="
                    Whitespace@5..6 " "
                    Literal@6..8
                      Integer@6..8 "10"
                error at 4..5: expected 'mut' or identifier, but found '='
            "#]],
        )
    }

    #[test]
    fn recover_on_let_token() {
        check_debug_tree_in_block(
            "let a =\nlet b = a",
            expect![[r#"
                SourceFile@0..17
                  Let@0..7
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Eq@6..7 "="
                  Whitespace@7..8 "\n"
                  Let@8..17
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
                error at 8..11: expected integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '!', '(', '{', 'if', 'return', 'loop', 'continue', 'break' or 'while', but found 'let'
            "#]],
        );
    }

    #[test]
    fn recover_on_fn_token() {
        check_debug_tree_in_block(
            "fn a(\nfn b() {}",
            expect![[r#"
                SourceFile@0..15
                  FunctionDef@0..5
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..4 "a"
                    ParamList@4..5
                      LParen@4..5 "("
                  Whitespace@5..6 "\n"
                  FunctionDef@6..15
                    FnKw@6..8 "fn"
                    Whitespace@8..9 " "
                    Ident@9..10 "b"
                    ParamList@10..12
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
        check_debug_tree_in_block(
            "let a = 1\na",
            expect![[r#"
                SourceFile@0..11
                  Let@0..9
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Eq@6..7 "="
                    Whitespace@7..8 " "
                    Literal@8..9
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
        check_debug_tree_in_block(
            "fn foo() -> int {}",
            expect![[r#"
                SourceFile@0..18
                  FunctionDef@0..18
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..8
                      LParen@6..7 "("
                      RParen@7..8 ")"
                    Whitespace@8..9 " "
                    ReturnType@9..15
                      ThinArrow@9..11 "->"
                      Whitespace@11..12 " "
                      Type@12..15
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
        check_debug_tree_in_block(
            "fn foo(a: int, b: int) -> int {}",
            expect![[r#"
                SourceFile@0..32
                  FunctionDef@0..32
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..22
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
                    ReturnType@23..29
                      ThinArrow@23..25 "->"
                      Whitespace@25..26 " "
                      Type@26..29
                        Ident@26..29 "int"
                    Whitespace@29..30 " "
                    BlockExpr@30..32
                      LCurly@30..31 "{"
                      RCurly@31..32 "}"
            "#]],
        );
    }

    #[test]
    fn parse_function_with_params_definition_mutable() {
        check_debug_tree_in_block(
            "fn foo(a: mut int, b: mut int) -> int {}",
            expect![[r#"
                SourceFile@0..40
                  FunctionDef@0..40
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..30
                      LParen@6..7 "("
                      Param@7..17
                        Ident@7..8 "a"
                        Colon@8..9 ":"
                        Whitespace@9..10 " "
                        MutKw@10..13 "mut"
                        Whitespace@13..14 " "
                        Type@14..17
                          Ident@14..17 "int"
                      Comma@17..18 ","
                      Whitespace@18..19 " "
                      Param@19..29
                        Ident@19..20 "b"
                        Colon@20..21 ":"
                        Whitespace@21..22 " "
                        MutKw@22..25 "mut"
                        Whitespace@25..26 " "
                        Type@26..29
                          Ident@26..29 "int"
                      RParen@29..30 ")"
                    Whitespace@30..31 " "
                    ReturnType@31..37
                      ThinArrow@31..33 "->"
                      Whitespace@33..34 " "
                      Type@34..37
                        Ident@34..37 "int"
                    Whitespace@37..38 " "
                    BlockExpr@38..40
                      LCurly@38..39 "{"
                      RCurly@39..40 "}"
            "#]],
        );
    }

    #[test]
    fn parse_function_with_block_definition() {
        check_debug_tree_in_block(
            "fn foo() -> int { 10 + 20 }",
            expect![[r#"
                SourceFile@0..27
                  FunctionDef@0..27
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..8
                      LParen@6..7 "("
                      RParen@7..8 ")"
                    Whitespace@8..9 " "
                    ReturnType@9..15
                      ThinArrow@9..11 "->"
                      Whitespace@11..12 " "
                      Type@12..15
                        Ident@12..15 "int"
                    Whitespace@15..16 " "
                    BlockExpr@16..27
                      LCurly@16..17 "{"
                      Whitespace@17..18 " "
                      ExprStmt@18..25
                        BinaryExpr@18..25
                          Literal@18..20
                            Integer@18..20 "10"
                          Whitespace@20..21 " "
                          Plus@21..22 "+"
                          Whitespace@22..23 " "
                          Literal@23..25
                            Integer@23..25 "20"
                      Whitespace@25..26 " "
                      RCurly@26..27 "}"
            "#]],
        );
    }

    #[test]
    fn parse_function_missing_param() {
        check_debug_tree_in_block(
            "fn foo(x: int, y -> int { 10 }",
            expect![[r#"
                SourceFile@0..30
                  FunctionDef@0..30
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..16
                      LParen@6..7 "("
                      Param@7..13
                        Ident@7..8 "x"
                        Colon@8..9 ":"
                        Whitespace@9..10 " "
                        Type@10..13
                          Ident@10..13 "int"
                      Comma@13..14 ","
                      Whitespace@14..15 " "
                      Param@15..16
                        Ident@15..16 "y"
                    Whitespace@16..17 " "
                    ReturnType@17..23
                      ThinArrow@17..19 "->"
                      Whitespace@19..20 " "
                      Type@20..23
                        Ident@20..23 "int"
                    Whitespace@23..24 " "
                    BlockExpr@24..30
                      LCurly@24..25 "{"
                      Whitespace@25..26 " "
                      ExprStmt@26..28
                        Literal@26..28
                          Integer@26..28 "10"
                      Whitespace@28..29 " "
                      RCurly@29..30 "}"
                error at 17..19: expected ':', but found ->
                error at 17..19: expected 'mut', identifier, ',' or ')', but found ->
            "#]],
        );

        check_debug_tree_in_block(
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
                    ReturnType@15..21
                      ThinArrow@15..17 "->"
                      Whitespace@17..18 " "
                      Type@18..21
                        Ident@18..21 "int"
                    Whitespace@21..22 " "
                    BlockExpr@22..28
                      LCurly@22..23 "{"
                      Whitespace@23..24 " "
                      ExprStmt@24..26
                        Literal@24..26
                          Integer@24..26 "10"
                      Whitespace@26..27 " "
                      RCurly@27..28 "}"
                error at 15..17: expected identifier, but found ->
                error at 15..17: expected ':', but found ->
                error at 15..17: expected 'mut', identifier, ',' or ')', but found ->
            "#]],
        );

        check_debug_tree_in_block(
            "fn foo(x: int -> int { 10 }",
            expect![[r#"
                SourceFile@0..27
                  FunctionDef@0..27
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..13
                      LParen@6..7 "("
                      Param@7..13
                        Ident@7..8 "x"
                        Colon@8..9 ":"
                        Whitespace@9..10 " "
                        Type@10..13
                          Ident@10..13 "int"
                    Whitespace@13..14 " "
                    ReturnType@14..20
                      ThinArrow@14..16 "->"
                      Whitespace@16..17 " "
                      Type@17..20
                        Ident@17..20 "int"
                    Whitespace@20..21 " "
                    BlockExpr@21..27
                      LCurly@21..22 "{"
                      Whitespace@22..23 " "
                      ExprStmt@23..25
                        Literal@23..25
                          Integer@23..25 "10"
                      Whitespace@25..26 " "
                      RCurly@26..27 "}"
                error at 14..16: expected ',' or ')', but found ->
            "#]],
        );

        check_debug_tree_in_block(
            "fn foo(x: -> int { 10 }",
            expect![[r#"
                SourceFile@0..23
                  FunctionDef@0..23
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..9
                      LParen@6..7 "("
                      Param@7..9
                        Ident@7..8 "x"
                        Colon@8..9 ":"
                    Whitespace@9..10 " "
                    ReturnType@10..16
                      ThinArrow@10..12 "->"
                      Whitespace@12..13 " "
                      Type@13..16
                        Ident@13..16 "int"
                    Whitespace@16..17 " "
                    BlockExpr@17..23
                      LCurly@17..18 "{"
                      Whitespace@18..19 " "
                      ExprStmt@19..21
                        Literal@19..21
                          Integer@19..21 "10"
                      Whitespace@21..22 " "
                      RCurly@22..23 "}"
                error at 10..12: expected 'mut', identifier, ',' or ')', but found ->
            "#]],
        );

        check_debug_tree_in_block(
            "fn foo(x -> int { 10 }",
            expect![[r#"
                SourceFile@0..22
                  FunctionDef@0..22
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..8
                      LParen@6..7 "("
                      Param@7..8
                        Ident@7..8 "x"
                    Whitespace@8..9 " "
                    ReturnType@9..15
                      ThinArrow@9..11 "->"
                      Whitespace@11..12 " "
                      Type@12..15
                        Ident@12..15 "int"
                    Whitespace@15..16 " "
                    BlockExpr@16..22
                      LCurly@16..17 "{"
                      Whitespace@17..18 " "
                      ExprStmt@18..20
                        Literal@18..20
                          Integer@18..20 "10"
                      Whitespace@20..21 " "
                      RCurly@21..22 "}"
                error at 9..11: expected ':', but found ->
                error at 9..11: expected 'mut', identifier, ',' or ')', but found ->
            "#]],
        );

        check_debug_tree_in_block(
            "fn foo( -> int { 10 }",
            expect![[r#"
                SourceFile@0..21
                  FunctionDef@0..21
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "foo"
                    ParamList@6..7
                      LParen@6..7 "("
                    Whitespace@7..8 " "
                    ReturnType@8..14
                      ThinArrow@8..10 "->"
                      Whitespace@10..11 " "
                      Type@11..14
                        Ident@11..14 "int"
                    Whitespace@14..15 " "
                    BlockExpr@15..21
                      LCurly@15..16 "{"
                      Whitespace@16..17 " "
                      ExprStmt@17..19
                        Literal@17..19
                          Integer@17..19 "10"
                      Whitespace@19..20 " "
                      RCurly@20..21 "}"
                error at 8..10: expected identifier or ')', but found ->
            "#]],
        );
    }

    #[test]
    fn parse_function_missing_ident() {
        check_debug_tree_in_block(
            "fn (a, b) -> int { 10 }",
            expect![[r#"
                SourceFile@0..23
                  FunctionDef@0..23
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    ParamList@3..9
                      LParen@3..4 "("
                      Param@4..5
                        Ident@4..5 "a"
                      Comma@5..6 ","
                      Whitespace@6..7 " "
                      Param@7..8
                        Ident@7..8 "b"
                      RParen@8..9 ")"
                    Whitespace@9..10 " "
                    ReturnType@10..16
                      ThinArrow@10..12 "->"
                      Whitespace@12..13 " "
                      Type@13..16
                        Ident@13..16 "int"
                    Whitespace@16..17 " "
                    BlockExpr@17..23
                      LCurly@17..18 "{"
                      Whitespace@18..19 " "
                      ExprStmt@19..21
                        Literal@19..21
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
        check_debug_tree_in_block(
            "fn a() { 10 }",
            expect![[r#"
                SourceFile@0..13
                  FunctionDef@0..13
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..4 "a"
                    ParamList@4..6
                      LParen@4..5 "("
                      RParen@5..6 ")"
                    Whitespace@6..7 " "
                    BlockExpr@7..13
                      LCurly@7..8 "{"
                      Whitespace@8..9 " "
                      ExprStmt@9..11
                        Literal@9..11
                          Integer@9..11 "10"
                      Whitespace@11..12 " "
                      RCurly@12..13 "}"
            "#]],
        );

        check_debug_tree_in_block(
            "fn a() -> { 10 }",
            expect![[r#"
                SourceFile@0..16
                  FunctionDef@0..16
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..4 "a"
                    ParamList@4..6
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
                      ExprStmt@12..14
                        Literal@12..14
                          Integer@12..14 "10"
                      Whitespace@14..15 " "
                      RCurly@15..16 "}"
                error at 10..11: expected identifier, but found '{'
            "#]],
        );

        check_debug_tree_in_block(
            "fn a() int { 10 }",
            expect![[r#"
                SourceFile@0..17
                  FunctionDef@0..6
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..4 "a"
                    ParamList@4..6
                      LParen@4..5 "("
                      RParen@5..6 ")"
                  Whitespace@6..7 " "
                  ExprStmt@7..10
                    PathExpr@7..10
                      Path@7..10
                        PathSegment@7..10
                          Ident@7..10 "int"
                  Whitespace@10..11 " "
                  ExprStmt@11..17
                    BlockExpr@11..17
                      LCurly@11..12 "{"
                      Whitespace@12..13 " "
                      ExprStmt@13..15
                        Literal@13..15
                          Integer@13..15 "10"
                      Whitespace@15..16 " "
                      RCurly@16..17 "}"
            "#]],
        );
    }

    #[test]
    fn parse_struct_definition_empty_fields() {
        check_debug_tree_in_block(
            r#"
struct AAA;
            "#,
            expect![[r#"
                SourceFile@0..25
                  Whitespace@0..1 "\n"
                  StructDef@1..12
                    StructKw@1..7 "struct"
                    Whitespace@7..8 " "
                    Ident@8..11 "AAA"
                    Semicolon@11..12 ";"
                  Whitespace@12..25 "\n            "
            "#]],
        );
    }

    #[test]
    fn parse_struct_definition_tuple_fields() {
        check_debug_tree_in_block(
            r#"
struct AAA();
struct AAA(int, int);
struct AAA(int, int,);
            "#,
            expect![[r#"
                SourceFile@0..72
                  Whitespace@0..1 "\n"
                  StructDef@1..14
                    StructKw@1..7 "struct"
                    Whitespace@7..8 " "
                    Ident@8..11 "AAA"
                    TupleFieldList@11..13
                      LParen@11..12 "("
                      RParen@12..13 ")"
                    Semicolon@13..14 ";"
                  Whitespace@14..15 "\n"
                  StructDef@15..36
                    StructKw@15..21 "struct"
                    Whitespace@21..22 " "
                    Ident@22..25 "AAA"
                    TupleFieldList@25..35
                      LParen@25..26 "("
                      TupleField@26..29
                        Ident@26..29 "int"
                      Comma@29..30 ","
                      Whitespace@30..31 " "
                      TupleField@31..34
                        Ident@31..34 "int"
                      RParen@34..35 ")"
                    Semicolon@35..36 ";"
                  Whitespace@36..37 "\n"
                  StructDef@37..59
                    StructKw@37..43 "struct"
                    Whitespace@43..44 " "
                    Ident@44..47 "AAA"
                    TupleFieldList@47..58
                      LParen@47..48 "("
                      TupleField@48..51
                        Ident@48..51 "int"
                      Comma@51..52 ","
                      Whitespace@52..53 " "
                      TupleField@53..56
                        Ident@53..56 "int"
                      Comma@56..57 ","
                      RParen@57..58 ")"
                    Semicolon@58..59 ";"
                  Whitespace@59..72 "\n            "
            "#]],
        );
    }

    #[test]
    fn parse_struct_definition_record_fields() {
        check_debug_tree_in_block(
            r#"
struct AAA {
}
struct AAA {
    a: int,
    b: int
}
struct AAA {
    a: int,
    b: int,
}
            "#,
            expect![[r#"
                SourceFile@0..105
                  Whitespace@0..1 "\n"
                  StructDef@1..15
                    StructKw@1..7 "struct"
                    Whitespace@7..8 " "
                    Ident@8..11 "AAA"
                    Whitespace@11..12 " "
                    RecordFieldList@12..15
                      LCurly@12..13 "{"
                      Whitespace@13..14 "\n"
                      RCurly@14..15 "}"
                  Whitespace@15..16 "\n"
                  StructDef@16..53
                    StructKw@16..22 "struct"
                    Whitespace@22..23 " "
                    Ident@23..26 "AAA"
                    Whitespace@26..27 " "
                    RecordFieldList@27..53
                      LCurly@27..28 "{"
                      Whitespace@28..33 "\n    "
                      RecordField@33..39
                        Ident@33..34 "a"
                        Colon@34..35 ":"
                        Whitespace@35..36 " "
                        Type@36..39
                          Ident@36..39 "int"
                      Comma@39..40 ","
                      Whitespace@40..45 "\n    "
                      RecordField@45..51
                        Ident@45..46 "b"
                        Colon@46..47 ":"
                        Whitespace@47..48 " "
                        Type@48..51
                          Ident@48..51 "int"
                      Whitespace@51..52 "\n"
                      RCurly@52..53 "}"
                  Whitespace@53..54 "\n"
                  StructDef@54..92
                    StructKw@54..60 "struct"
                    Whitespace@60..61 " "
                    Ident@61..64 "AAA"
                    Whitespace@64..65 " "
                    RecordFieldList@65..92
                      LCurly@65..66 "{"
                      Whitespace@66..71 "\n    "
                      RecordField@71..77
                        Ident@71..72 "a"
                        Colon@72..73 ":"
                        Whitespace@73..74 " "
                        Type@74..77
                          Ident@74..77 "int"
                      Comma@77..78 ","
                      Whitespace@78..83 "\n    "
                      RecordField@83..89
                        Ident@83..84 "b"
                        Colon@84..85 ":"
                        Whitespace@85..86 " "
                        Type@86..89
                          Ident@86..89 "int"
                      Comma@89..90 ","
                      Whitespace@90..91 "\n"
                      RCurly@91..92 "}"
                  Whitespace@92..105 "\n            "
            "#]],
        );
    }

    #[test]
    fn parse_struct_definition_missing() {
        check_debug_tree_in_block(
            r#"
struct ;
            "#,
            expect![[r#"
                SourceFile@0..22
                  Whitespace@0..1 "\n"
                  StructDef@1..9
                    StructKw@1..7 "struct"
                    Whitespace@7..8 " "
                    Semicolon@8..9 ";"
                  Whitespace@9..22 "\n            "
                error at 8..9: expected identifier, but found ';'
            "#]],
        );

        check_debug_tree_in_block(
            r#"
struct ;
            "#,
            expect![[r#"
                SourceFile@0..22
                  Whitespace@0..1 "\n"
                  StructDef@1..9
                    StructKw@1..7 "struct"
                    Whitespace@7..8 " "
                    Semicolon@8..9 ";"
                  Whitespace@9..22 "\n            "
                error at 8..9: expected identifier, but found ';'
            "#]],
        );

        check_debug_tree_in_block(
            r#"
struct (i32);
            "#,
            expect![[r#"
                SourceFile@0..27
                  Whitespace@0..1 "\n"
                  StructDef@1..14
                    StructKw@1..7 "struct"
                    Whitespace@7..8 " "
                    TupleFieldList@8..13
                      LParen@8..9 "("
                      TupleField@9..12
                        Ident@9..12 "i32"
                      RParen@12..13 ")"
                    Semicolon@13..14 ";"
                  Whitespace@14..27 "\n            "
                error at 8..9: expected identifier, but found '('
            "#]],
        );

        check_debug_tree_in_block(
            r#"
struct { a: i32 }
            "#,
            expect![[r#"
                SourceFile@0..31
                  Whitespace@0..1 "\n"
                  StructDef@1..18
                    StructKw@1..7 "struct"
                    Whitespace@7..8 " "
                    RecordFieldList@8..18
                      LCurly@8..9 "{"
                      Whitespace@9..10 " "
                      RecordField@10..16
                        Ident@10..11 "a"
                        Colon@11..12 ":"
                        Whitespace@12..13 " "
                        Type@13..16
                          Ident@13..16 "i32"
                      Whitespace@16..17 " "
                      RCurly@17..18 "}"
                  Whitespace@18..31 "\n            "
                error at 8..9: expected identifier, but found '{'
            "#]],
        );

        check_debug_tree_in_block(
            r#"
struct AAA { : i32, : bool }
            "#,
            expect![[r#"
                SourceFile@0..42
                  Whitespace@0..1 "\n"
                  StructDef@1..29
                    StructKw@1..7 "struct"
                    Whitespace@7..8 " "
                    Ident@8..11 "AAA"
                    Whitespace@11..12 " "
                    RecordFieldList@12..29
                      LCurly@12..13 "{"
                      Whitespace@13..14 " "
                      RecordField@14..19
                        Colon@14..15 ":"
                        Whitespace@15..16 " "
                        Type@16..19
                          Ident@16..19 "i32"
                      Comma@19..20 ","
                      Whitespace@20..21 " "
                      RecordField@21..27
                        Colon@21..22 ":"
                        Whitespace@22..23 " "
                        Type@23..27
                          Ident@23..27 "bool"
                      Whitespace@27..28 " "
                      RCurly@28..29 "}"
                  Whitespace@29..42 "\n            "
                error at 14..15: expected identifier or ':', but found ':'
                error at 21..22: expected '}' or identifier, but found ':'
            "#]],
        );

        check_debug_tree_in_block(
            r#"
struct AAA { a:, b: }
            "#,
            expect![[r#"
                SourceFile@0..35
                  Whitespace@0..1 "\n"
                  StructDef@1..22
                    StructKw@1..7 "struct"
                    Whitespace@7..8 " "
                    Ident@8..11 "AAA"
                    Whitespace@11..12 " "
                    RecordFieldList@12..22
                      LCurly@12..13 "{"
                      Whitespace@13..14 " "
                      RecordField@14..16
                        Ident@14..15 "a"
                        Colon@15..16 ":"
                      Comma@16..17 ","
                      Whitespace@17..18 " "
                      RecordField@18..20
                        Ident@18..19 "b"
                        Colon@19..20 ":"
                      Whitespace@20..21 " "
                      RCurly@21..22 "}"
                  Whitespace@22..35 "\n            "
            "#]],
        );
    }
}
