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

    // TODO: parse path type(ex. : foo::bar::i32)

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
                super::parse_path_type(parser, recovery_set);
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
                    super::parse_path_type(parser, recovery_set);
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

    if parser.at(TokenKind::Ident) {
        super::parse_path_type(parser, recovery_set);
    } else {
        parser.error_with_recovery_set_no_default(recovery_set);
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

    if parser.at(TokenKind::Ident)
        || parser.peek() == Some(TokenKind::Colon)
        || parser.peek() == Some(TokenKind::Comma)
    {
        {
            let marker = parser.start();
            parser.expect_with_recovery_set_no_default(TokenKind::Ident, recovery_set);
            parser.expect_with_recovery_set_no_default(TokenKind::Colon, recovery_set);

            if parser.at(TokenKind::Ident) {
                super::parse_path_type(parser, recovery_set);
            }
            marker.complete(parser, SyntaxKind::RecordField);
        }
        while parser.at(TokenKind::Comma) {
            parser.bump();
            // struct AAA { a: int, b: int, }
            //                            ^,時点で停止させる
            if parser.at(TokenKind::RCurly) {
                break;
            }

            {
                let marker = parser.start();
                parser.expect_with_recovery_set_no_default(TokenKind::Ident, recovery_set);
                parser.expect_with_recovery_set_no_default(TokenKind::Colon, recovery_set);

                if parser.at(TokenKind::Ident) {
                    super::parse_path_type(parser, recovery_set);
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
            super::parse_path_type(parser, recovery_set);
            marker.complete(parser, SyntaxKind::TupleField);
        }
        while parser.at(TokenKind::Comma) {
            parser.bump();
            // struct AAA(i32, i32,);
            //                    ^,時点で停止させる(`(`は`parse_struct`側でbump)
            if parser.at(TokenKind::RParen) {
                break;
            }

            {
                let marker = parser.start();
                if parser.at(TokenKind::Ident) {
                    super::parse_path_type(parser, recovery_set);
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
                error at 8..10: expected integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '!', '[', '(', '{', 'if', 'return', 'loop', 'continue', 'break' or 'while'
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
                error at 8..11: expected integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '!', '[', '(', '{', 'if', 'return', 'loop', 'continue', 'break' or 'while', but found 'let'
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
                      PathType@12..15
                        Path@12..15
                          PathSegment@12..15
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
                        PathType@10..13
                          Path@10..13
                            PathSegment@10..13
                              Ident@10..13 "int"
                      Comma@13..14 ","
                      Whitespace@14..15 " "
                      Param@15..21
                        Ident@15..16 "b"
                        Colon@16..17 ":"
                        Whitespace@17..18 " "
                        PathType@18..21
                          Path@18..21
                            PathSegment@18..21
                              Ident@18..21 "int"
                      RParen@21..22 ")"
                    Whitespace@22..23 " "
                    ReturnType@23..29
                      ThinArrow@23..25 "->"
                      Whitespace@25..26 " "
                      PathType@26..29
                        Path@26..29
                          PathSegment@26..29
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
                        PathType@14..17
                          Path@14..17
                            PathSegment@14..17
                              Ident@14..17 "int"
                      Comma@17..18 ","
                      Whitespace@18..19 " "
                      Param@19..29
                        Ident@19..20 "b"
                        Colon@20..21 ":"
                        Whitespace@21..22 " "
                        MutKw@22..25 "mut"
                        Whitespace@25..26 " "
                        PathType@26..29
                          Path@26..29
                            PathSegment@26..29
                              Ident@26..29 "int"
                      RParen@29..30 ")"
                    Whitespace@30..31 " "
                    ReturnType@31..37
                      ThinArrow@31..33 "->"
                      Whitespace@33..34 " "
                      PathType@34..37
                        Path@34..37
                          PathSegment@34..37
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
                      PathType@12..15
                        Path@12..15
                          PathSegment@12..15
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
                        PathType@10..13
                          Path@10..13
                            PathSegment@10..13
                              Ident@10..13 "int"
                      Comma@13..14 ","
                      Whitespace@14..15 " "
                      Param@15..16
                        Ident@15..16 "y"
                    Whitespace@16..17 " "
                    ReturnType@17..23
                      ThinArrow@17..19 "->"
                      Whitespace@19..20 " "
                      PathType@20..23
                        Path@20..23
                          PathSegment@20..23
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
                        PathType@10..13
                          Path@10..13
                            PathSegment@10..13
                              Ident@10..13 "int"
                      Comma@13..14 ","
                      Whitespace@14..15 " "
                      Param@15..15
                    ReturnType@15..21
                      ThinArrow@15..17 "->"
                      Whitespace@17..18 " "
                      PathType@18..21
                        Path@18..21
                          PathSegment@18..21
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
                        PathType@10..13
                          Path@10..13
                            PathSegment@10..13
                              Ident@10..13 "int"
                    Whitespace@13..14 " "
                    ReturnType@14..20
                      ThinArrow@14..16 "->"
                      Whitespace@16..17 " "
                      PathType@17..20
                        Path@17..20
                          PathSegment@17..20
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
                error at 14..16: expected '::', ',' or ')', but found ->
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
                      PathType@13..16
                        Path@13..16
                          PathSegment@13..16
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
                      PathType@12..15
                        Path@12..15
                          PathSegment@12..15
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
                      PathType@11..14
                        Path@11..14
                          PathSegment@11..14
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
                      PathType@13..16
                        Path@13..16
                          PathSegment@13..16
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
                    ReturnType@7..9
                      ThinArrow@7..9 "->"
                    Whitespace@9..10 " "
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
                  ExprStmt@7..15
                    RecordExpr@7..15
                      Path@7..10
                        PathSegment@7..10
                          Ident@7..10 "int"
                      Whitespace@10..11 " "
                      RecordFieldListExpr@11..15
                        LCurly@11..12 "{"
                        Whitespace@12..13 " "
                        Error@13..15
                          Integer@13..15 "10"
                  Whitespace@15..16 " "
                  ExprStmt@16..17
                    Error@16..17
                      RCurly@16..17 "}"
                error at 13..15: expected identifier or '}', but found integerLiteral
                error at 16..17: expected '+', '-', '*', '/', '==', '!=', '>', '<', '>=', '<=', '=', ';', 'let', 'fn', 'struct', 'mod', integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '!', '[', '(', '{', 'if', 'return', 'loop', 'continue', 'break' or 'while', but found '}'
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
            "struct AAA();",
            expect![[r#"
                SourceFile@0..13
                  StructDef@0..13
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    TupleFieldList@10..12
                      LParen@10..11 "("
                      RParen@11..12 ")"
                    Semicolon@12..13 ";"
            "#]],
        );

        check_debug_tree_in_block(
            "struct AAA(int);",
            expect![[r#"
                SourceFile@0..16
                  StructDef@0..16
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    TupleFieldList@10..15
                      LParen@10..11 "("
                      TupleField@11..14
                        PathType@11..14
                          Path@11..14
                            PathSegment@11..14
                              Ident@11..14 "int"
                      RParen@14..15 ")"
                    Semicolon@15..16 ";"
            "#]],
        );

        check_debug_tree_in_block(
            "struct AAA(int,);",
            expect![[r#"
                SourceFile@0..17
                  StructDef@0..17
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    TupleFieldList@10..16
                      LParen@10..11 "("
                      TupleField@11..14
                        PathType@11..14
                          Path@11..14
                            PathSegment@11..14
                              Ident@11..14 "int"
                      Comma@14..15 ","
                      RParen@15..16 ")"
                    Semicolon@16..17 ";"
            "#]],
        );

        check_debug_tree_in_block(
            "struct AAA(int, int);",
            expect![[r#"
                SourceFile@0..21
                  StructDef@0..21
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    TupleFieldList@10..20
                      LParen@10..11 "("
                      TupleField@11..14
                        PathType@11..14
                          Path@11..14
                            PathSegment@11..14
                              Ident@11..14 "int"
                      Comma@14..15 ","
                      Whitespace@15..16 " "
                      TupleField@16..19
                        PathType@16..19
                          Path@16..19
                            PathSegment@16..19
                              Ident@16..19 "int"
                      RParen@19..20 ")"
                    Semicolon@20..21 ";"
            "#]],
        );
        check_debug_tree_in_block(
            "struct AAA(int, int,);",
            expect![[r#"
                SourceFile@0..22
                  StructDef@0..22
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    TupleFieldList@10..21
                      LParen@10..11 "("
                      TupleField@11..14
                        PathType@11..14
                          Path@11..14
                            PathSegment@11..14
                              Ident@11..14 "int"
                      Comma@14..15 ","
                      Whitespace@15..16 " "
                      TupleField@16..19
                        PathType@16..19
                          Path@16..19
                            PathSegment@16..19
                              Ident@16..19 "int"
                      Comma@19..20 ","
                      RParen@20..21 ")"
                    Semicolon@21..22 ";"
            "#]],
        );
    }

    #[test]
    fn parse_struct_definition_record_fields() {
        check_debug_tree_in_block(
            "struct AAA {}",
            expect![[r#"
                SourceFile@0..13
                  StructDef@0..13
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    Whitespace@10..11 " "
                    RecordFieldList@11..13
                      LCurly@11..12 "{"
                      RCurly@12..13 "}"
            "#]],
        );

        check_debug_tree_in_block(
            "struct AAA { a: int }",
            expect![[r#"
                SourceFile@0..21
                  StructDef@0..21
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    Whitespace@10..11 " "
                    RecordFieldList@11..21
                      LCurly@11..12 "{"
                      Whitespace@12..13 " "
                      RecordField@13..19
                        Ident@13..14 "a"
                        Colon@14..15 ":"
                        Whitespace@15..16 " "
                        PathType@16..19
                          Path@16..19
                            PathSegment@16..19
                              Ident@16..19 "int"
                      Whitespace@19..20 " "
                      RCurly@20..21 "}"
            "#]],
        );

        check_debug_tree_in_block(
            "struct AAA { a: int, }",
            expect![[r#"
                SourceFile@0..22
                  StructDef@0..22
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    Whitespace@10..11 " "
                    RecordFieldList@11..22
                      LCurly@11..12 "{"
                      Whitespace@12..13 " "
                      RecordField@13..19
                        Ident@13..14 "a"
                        Colon@14..15 ":"
                        Whitespace@15..16 " "
                        PathType@16..19
                          Path@16..19
                            PathSegment@16..19
                              Ident@16..19 "int"
                      Comma@19..20 ","
                      Whitespace@20..21 " "
                      RCurly@21..22 "}"
            "#]],
        );

        check_debug_tree_in_block(
            "struct AAA { a: int, b: int }",
            expect![[r#"
                SourceFile@0..29
                  StructDef@0..29
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    Whitespace@10..11 " "
                    RecordFieldList@11..29
                      LCurly@11..12 "{"
                      Whitespace@12..13 " "
                      RecordField@13..19
                        Ident@13..14 "a"
                        Colon@14..15 ":"
                        Whitespace@15..16 " "
                        PathType@16..19
                          Path@16..19
                            PathSegment@16..19
                              Ident@16..19 "int"
                      Comma@19..20 ","
                      Whitespace@20..21 " "
                      RecordField@21..27
                        Ident@21..22 "b"
                        Colon@22..23 ":"
                        Whitespace@23..24 " "
                        PathType@24..27
                          Path@24..27
                            PathSegment@24..27
                              Ident@24..27 "int"
                      Whitespace@27..28 " "
                      RCurly@28..29 "}"
            "#]],
        );

        check_debug_tree_in_block(
            "struct AAA { a: int, b: int, }",
            expect![[r#"
                SourceFile@0..30
                  StructDef@0..30
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    Whitespace@10..11 " "
                    RecordFieldList@11..30
                      LCurly@11..12 "{"
                      Whitespace@12..13 " "
                      RecordField@13..19
                        Ident@13..14 "a"
                        Colon@14..15 ":"
                        Whitespace@15..16 " "
                        PathType@16..19
                          Path@16..19
                            PathSegment@16..19
                              Ident@16..19 "int"
                      Comma@19..20 ","
                      Whitespace@20..21 " "
                      RecordField@21..27
                        Ident@21..22 "b"
                        Colon@22..23 ":"
                        Whitespace@23..24 " "
                        PathType@24..27
                          Path@24..27
                            PathSegment@24..27
                              Ident@24..27 "int"
                      Comma@27..28 ","
                      Whitespace@28..29 " "
                      RCurly@29..30 "}"
            "#]],
        );
    }

    #[test]
    fn parse_struct_definition_missing() {
        check_debug_tree_in_block(
            "struct ;",
            expect![[r#"
                SourceFile@0..8
                  StructDef@0..8
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Semicolon@7..8 ";"
                error at 7..8: expected identifier, but found ';'
            "#]],
        );

        check_debug_tree_in_block(
            "struct ;",
            expect![[r#"
                SourceFile@0..8
                  StructDef@0..8
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Semicolon@7..8 ";"
                error at 7..8: expected identifier, but found ';'
            "#]],
        );

        check_debug_tree_in_block(
            "struct (i32);",
            expect![[r#"
                SourceFile@0..13
                  StructDef@0..13
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    TupleFieldList@7..12
                      LParen@7..8 "("
                      TupleField@8..11
                        PathType@8..11
                          Path@8..11
                            PathSegment@8..11
                              Ident@8..11 "i32"
                      RParen@11..12 ")"
                    Semicolon@12..13 ";"
                error at 7..8: expected identifier, but found '('
            "#]],
        );

        check_debug_tree_in_block(
            "struct { a: i32 }",
            expect![[r#"
                SourceFile@0..17
                  StructDef@0..17
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    RecordFieldList@7..17
                      LCurly@7..8 "{"
                      Whitespace@8..9 " "
                      RecordField@9..15
                        Ident@9..10 "a"
                        Colon@10..11 ":"
                        Whitespace@11..12 " "
                        PathType@12..15
                          Path@12..15
                            PathSegment@12..15
                              Ident@12..15 "i32"
                      Whitespace@15..16 " "
                      RCurly@16..17 "}"
                error at 7..8: expected identifier, but found '{'
            "#]],
        );

        check_debug_tree_in_block(
            "struct AAA { :, : }",
            expect![[r#"
                SourceFile@0..19
                  StructDef@0..19
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    Whitespace@10..11 " "
                    RecordFieldList@11..19
                      LCurly@11..12 "{"
                      Whitespace@12..13 " "
                      RecordField@13..14
                        Colon@13..14 ":"
                      Comma@14..15 ","
                      Whitespace@15..16 " "
                      RecordField@16..17
                        Colon@16..17 ":"
                      Whitespace@17..18 " "
                      RCurly@18..19 "}"
                error at 13..14: expected identifier, but found ':'
                error at 16..17: expected '}' or identifier, but found ':'
            "#]],
        );

        check_debug_tree_in_block(
            "struct AAA { : i32, : bool }",
            expect![[r#"
                SourceFile@0..28
                  StructDef@0..28
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    Whitespace@10..11 " "
                    RecordFieldList@11..28
                      LCurly@11..12 "{"
                      Whitespace@12..13 " "
                      RecordField@13..18
                        Colon@13..14 ":"
                        Whitespace@14..15 " "
                        PathType@15..18
                          Path@15..18
                            PathSegment@15..18
                              Ident@15..18 "i32"
                      Comma@18..19 ","
                      Whitespace@19..20 " "
                      RecordField@20..26
                        Colon@20..21 ":"
                        Whitespace@21..22 " "
                        PathType@22..26
                          Path@22..26
                            PathSegment@22..26
                              Ident@22..26 "bool"
                      Whitespace@26..27 " "
                      RCurly@27..28 "}"
                error at 13..14: expected identifier, but found ':'
                error at 20..21: expected '}' or identifier, but found ':'
            "#]],
        );

        check_debug_tree_in_block(
            "struct AAA { a:, b: }",
            expect![[r#"
                SourceFile@0..21
                  StructDef@0..21
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    Whitespace@10..11 " "
                    RecordFieldList@11..21
                      LCurly@11..12 "{"
                      Whitespace@12..13 " "
                      RecordField@13..15
                        Ident@13..14 "a"
                        Colon@14..15 ":"
                      Comma@15..16 ","
                      Whitespace@16..17 " "
                      RecordField@17..19
                        Ident@17..18 "b"
                        Colon@18..19 ":"
                      Whitespace@19..20 " "
                      RCurly@20..21 "}"
            "#]],
        );
    }

    #[test]
    fn path_type_in_function() {
        check_debug_tree_in_block(
            "fn add(x: foo::bar, y: foo::bar) -> foo::bar {}",
            expect![[r#"
                SourceFile@0..47
                  FunctionDef@0..47
                    FnKw@0..2 "fn"
                    Whitespace@2..3 " "
                    Ident@3..6 "add"
                    ParamList@6..32
                      LParen@6..7 "("
                      Param@7..18
                        Ident@7..8 "x"
                        Colon@8..9 ":"
                        Whitespace@9..10 " "
                        PathType@10..18
                          Path@10..18
                            PathSegment@10..13
                              Ident@10..13 "foo"
                            Colon2@13..15 "::"
                            PathSegment@15..18
                              Ident@15..18 "bar"
                      Comma@18..19 ","
                      Whitespace@19..20 " "
                      Param@20..31
                        Ident@20..21 "y"
                        Colon@21..22 ":"
                        Whitespace@22..23 " "
                        PathType@23..31
                          Path@23..31
                            PathSegment@23..26
                              Ident@23..26 "foo"
                            Colon2@26..28 "::"
                            PathSegment@28..31
                              Ident@28..31 "bar"
                      RParen@31..32 ")"
                    Whitespace@32..33 " "
                    ReturnType@33..44
                      ThinArrow@33..35 "->"
                      Whitespace@35..36 " "
                      PathType@36..44
                        Path@36..44
                          PathSegment@36..39
                            Ident@36..39 "foo"
                          Colon2@39..41 "::"
                          PathSegment@41..44
                            Ident@41..44 "bar"
                    Whitespace@44..45 " "
                    BlockExpr@45..47
                      LCurly@45..46 "{"
                      RCurly@46..47 "}"
            "#]],
        );
    }

    #[test]
    fn path_type_in_struct() {
        check_debug_tree_in_block(
            "struct AAA(foo::bar, aaa::bbb);",
            expect![[r#"
                SourceFile@0..31
                  StructDef@0..31
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    TupleFieldList@10..30
                      LParen@10..11 "("
                      TupleField@11..19
                        PathType@11..19
                          Path@11..19
                            PathSegment@11..14
                              Ident@11..14 "foo"
                            Colon2@14..16 "::"
                            PathSegment@16..19
                              Ident@16..19 "bar"
                      Comma@19..20 ","
                      Whitespace@20..21 " "
                      TupleField@21..29
                        PathType@21..29
                          Path@21..29
                            PathSegment@21..24
                              Ident@21..24 "aaa"
                            Colon2@24..26 "::"
                            PathSegment@26..29
                              Ident@26..29 "bbb"
                      RParen@29..30 ")"
                    Semicolon@30..31 ";"
            "#]],
        );

        check_debug_tree_in_block(
            "struct AAA { x: foo::bar, y: aaa::bbb }",
            expect![[r#"
                SourceFile@0..39
                  StructDef@0..39
                    StructKw@0..6 "struct"
                    Whitespace@6..7 " "
                    Ident@7..10 "AAA"
                    Whitespace@10..11 " "
                    RecordFieldList@11..39
                      LCurly@11..12 "{"
                      Whitespace@12..13 " "
                      RecordField@13..24
                        Ident@13..14 "x"
                        Colon@14..15 ":"
                        Whitespace@15..16 " "
                        PathType@16..24
                          Path@16..24
                            PathSegment@16..19
                              Ident@16..19 "foo"
                            Colon2@19..21 "::"
                            PathSegment@21..24
                              Ident@21..24 "bar"
                      Comma@24..25 ","
                      Whitespace@25..26 " "
                      RecordField@26..37
                        Ident@26..27 "y"
                        Colon@27..28 ":"
                        Whitespace@28..29 " "
                        PathType@29..37
                          Path@29..37
                            PathSegment@29..32
                              Ident@29..32 "aaa"
                            Colon2@32..34 "::"
                            PathSegment@34..37
                              Ident@34..37 "bbb"
                      Whitespace@37..38 " "
                      RCurly@38..39 "}"
            "#]],
        );
    }
}
