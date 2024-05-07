use lexer::TokenKind;
use syntax::SyntaxKind;

use super::stmt;
use crate::{
    grammar::{expr::ITEM_FIRST, parse_path},
    parser::{marker::CompletedNodeMarker, Parser, TOPLEVEL_RECOVERY_SET},
};

/// トップレベルでのステートメントのパースを行います。
pub(super) fn parse_stmt_on_toplevel(parser: &mut Parser) -> Option<CompletedNodeMarker> {
    if parser.at(TokenKind::FnKw) {
        Some(stmt::parse_function_def(parser, &TOPLEVEL_RECOVERY_SET))
    } else if parser.at(TokenKind::StructKw) {
        Some(stmt::parse_struct(parser, &TOPLEVEL_RECOVERY_SET))
    } else if parser.at(TokenKind::ModKw) {
        Some(parse_module(parser, &TOPLEVEL_RECOVERY_SET))
    } else if parser.at(TokenKind::UseKw) {
        Some(parse_use(parser, &TOPLEVEL_RECOVERY_SET))
    } else {
        parser.error_with_recovery_set_only_default_on_toplevel();
        None
    }
}

/// モジュールのパースを行います。
pub(super) fn parse_module(parser: &mut Parser, recovery_set: &[TokenKind]) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::ModKw));

    let marker = parser.start();
    parser.bump();

    parser.expect_with_recovery_set_no_default(TokenKind::Ident, recovery_set);

    // file module
    if parser.at(TokenKind::Semicolon) {
        parser.bump();
        return marker.complete(parser, SyntaxKind::Module);
    }

    // or

    // inline module
    {
        let marker = parser.start();
        if parser.at(TokenKind::LCurly) {
            parser.bump();

            while parser.at_set_no_expected(ITEM_FIRST) {
                parse_stmt_on_toplevel(parser);
            }
        }

        if parser.at(TokenKind::RCurly) {
            parser.bump();
        }

        marker.complete(parser, SyntaxKind::ItemList);
    }

    marker.complete(parser, SyntaxKind::Module)
}

/// 使用宣言のパースを行います。
pub(crate) fn parse_use(parser: &mut Parser, _recovery_set: &[TokenKind]) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::UseKw));

    let marker = parser.start();
    parser.bump();

    parse_path(parser);

    if parser.at(TokenKind::Semicolon) {
        parser.bump();
    }

    marker.complete(parser, SyntaxKind::Use)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check_debug_tree;

    #[test]
    fn parse_let() {
        check_debug_tree(
            "let foo = bar",
            expect![[r#"
                SourceFile@0..13
                  Error@0..3
                    LetKw@0..3 "let"
                  Whitespace@3..4 " "
                  Error@4..7
                    Ident@4..7 "foo"
                  Whitespace@7..8 " "
                  Error@8..9
                    Eq@8..9 "="
                  Whitespace@9..10 " "
                  Error@10..13
                    Ident@10..13 "bar"
                error at 0..3: expected 'fn', 'struct', 'mod' or 'use', but found 'let'
                error at 4..7: expected 'fn', 'struct', 'mod' or 'use', but found identifier
                error at 8..9: expected 'fn', 'struct', 'mod' or 'use', but found '='
                error at 10..13: expected 'fn', 'struct', 'mod' or 'use', but found identifier
            "#]],
        );
    }

    #[test]
    fn parse_function_definition() {
        check_debug_tree(
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
    fn parse_function_with_block_definition() {
        check_debug_tree(
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
        check_debug_tree(
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

        check_debug_tree(
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

        check_debug_tree(
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

        check_debug_tree(
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

        check_debug_tree(
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

        check_debug_tree(
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
        check_debug_tree(
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
        check_debug_tree(
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

        check_debug_tree(
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

        check_debug_tree(
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
                  Error@7..10
                    Ident@7..10 "int"
                  Whitespace@10..11 " "
                  Error@11..12
                    LCurly@11..12 "{"
                  Whitespace@12..13 " "
                  Error@13..15
                    Integer@13..15 "10"
                  Whitespace@15..16 " "
                  Error@16..17
                    RCurly@16..17 "}"
                error at 7..10: expected ->, '{', 'fn', 'struct', 'mod' or 'use', but found identifier
                error at 11..12: expected 'fn', 'struct', 'mod' or 'use', but found '{'
                error at 13..15: expected 'fn', 'struct', 'mod' or 'use', but found integerLiteral
                error at 16..17: expected 'fn', 'struct', 'mod' or 'use', but found '}'
            "#]],
        );
    }

    // Structの詳細は`stmt.rs`側で行う
    #[test]
    fn parse_struct_definition() {
        check_debug_tree(
            r#"
struct A;
struct A(i32, i32);
struct A { foo: i32, bar: i32 }
            "#,
            expect![[r#"
                SourceFile@0..75
                  Whitespace@0..1 "\n"
                  StructDef@1..10
                    StructKw@1..7 "struct"
                    Whitespace@7..8 " "
                    Ident@8..9 "A"
                    Semicolon@9..10 ";"
                  Whitespace@10..11 "\n"
                  StructDef@11..30
                    StructKw@11..17 "struct"
                    Whitespace@17..18 " "
                    Ident@18..19 "A"
                    TupleFieldList@19..29
                      LParen@19..20 "("
                      TupleField@20..23
                        Ident@20..23 "i32"
                      Comma@23..24 ","
                      Whitespace@24..25 " "
                      TupleField@25..28
                        Ident@25..28 "i32"
                      RParen@28..29 ")"
                    Semicolon@29..30 ";"
                  Whitespace@30..31 "\n"
                  StructDef@31..62
                    StructKw@31..37 "struct"
                    Whitespace@37..38 " "
                    Ident@38..39 "A"
                    Whitespace@39..40 " "
                    RecordFieldList@40..62
                      LCurly@40..41 "{"
                      Whitespace@41..42 " "
                      RecordField@42..50
                        Ident@42..45 "foo"
                        Colon@45..46 ":"
                        Whitespace@46..47 " "
                        Type@47..50
                          Ident@47..50 "i32"
                      Comma@50..51 ","
                      Whitespace@51..52 " "
                      RecordField@52..60
                        Ident@52..55 "bar"
                        Colon@55..56 ":"
                        Whitespace@56..57 " "
                        Type@57..60
                          Ident@57..60 "i32"
                      Whitespace@60..61 " "
                      RCurly@61..62 "}"
                  Whitespace@62..75 "\n            "
            "#]],
        );
    }

    #[test]
    fn parse_module() {
        check_debug_tree(
            r#"
mod one_module {
    fn bbb() -> int {
        10
    }
}
fn main() {}
            "#,
            expect![[r#"
                SourceFile@0..84
                  Whitespace@0..1 "\n"
                  Module@1..58
                    ModKw@1..4 "mod"
                    Whitespace@4..5 " "
                    Ident@5..15 "one_module"
                    Whitespace@15..16 " "
                    ItemList@16..58
                      LCurly@16..17 "{"
                      Whitespace@17..22 "\n    "
                      FunctionDef@22..56
                        FnKw@22..24 "fn"
                        Whitespace@24..25 " "
                        Ident@25..28 "bbb"
                        ParamList@28..30
                          LParen@28..29 "("
                          RParen@29..30 ")"
                        Whitespace@30..31 " "
                        ReturnType@31..37
                          ThinArrow@31..33 "->"
                          Whitespace@33..34 " "
                          Type@34..37
                            Ident@34..37 "int"
                        Whitespace@37..38 " "
                        BlockExpr@38..56
                          LCurly@38..39 "{"
                          Whitespace@39..48 "\n        "
                          ExprStmt@48..50
                            Literal@48..50
                              Integer@48..50 "10"
                          Whitespace@50..55 "\n    "
                          RCurly@55..56 "}"
                      Whitespace@56..57 "\n"
                      RCurly@57..58 "}"
                  Whitespace@58..59 "\n"
                  FunctionDef@59..71
                    FnKw@59..61 "fn"
                    Whitespace@61..62 " "
                    Ident@62..66 "main"
                    ParamList@66..68
                      LParen@66..67 "("
                      RParen@67..68 ")"
                    Whitespace@68..69 " "
                    BlockExpr@69..71
                      LCurly@69..70 "{"
                      RCurly@70..71 "}"
                  Whitespace@71..84 "\n            "
            "#]],
        );
    }

    #[test]
    fn parse_module_in_block() {
        check_debug_tree(
            r#"
mod one_module {
    mod second_module {
        fn aaa() -> int {
            10
        }
    }
    fn bbb() -> int {
        mod third_module {
            fn ccc() -> int {
                20
            }
        }
    }
}
fn main() {}
            "#,
            expect![[r#"
                SourceFile@0..254
                  Whitespace@0..1 "\n"
                  Module@1..228
                    ModKw@1..4 "mod"
                    Whitespace@4..5 " "
                    Ident@5..15 "one_module"
                    Whitespace@15..16 " "
                    ItemList@16..228
                      LCurly@16..17 "{"
                      Whitespace@17..22 "\n    "
                      Module@22..98
                        ModKw@22..25 "mod"
                        Whitespace@25..26 " "
                        Ident@26..39 "second_module"
                        Whitespace@39..40 " "
                        ItemList@40..98
                          LCurly@40..41 "{"
                          Whitespace@41..50 "\n        "
                          FunctionDef@50..92
                            FnKw@50..52 "fn"
                            Whitespace@52..53 " "
                            Ident@53..56 "aaa"
                            ParamList@56..58
                              LParen@56..57 "("
                              RParen@57..58 ")"
                            Whitespace@58..59 " "
                            ReturnType@59..65
                              ThinArrow@59..61 "->"
                              Whitespace@61..62 " "
                              Type@62..65
                                Ident@62..65 "int"
                            Whitespace@65..66 " "
                            BlockExpr@66..92
                              LCurly@66..67 "{"
                              Whitespace@67..80 "\n            "
                              ExprStmt@80..82
                                Literal@80..82
                                  Integer@80..82 "10"
                              Whitespace@82..91 "\n        "
                              RCurly@91..92 "}"
                          Whitespace@92..97 "\n    "
                          RCurly@97..98 "}"
                      Whitespace@98..103 "\n    "
                      FunctionDef@103..226
                        FnKw@103..105 "fn"
                        Whitespace@105..106 " "
                        Ident@106..109 "bbb"
                        ParamList@109..111
                          LParen@109..110 "("
                          RParen@110..111 ")"
                        Whitespace@111..112 " "
                        ReturnType@112..118
                          ThinArrow@112..114 "->"
                          Whitespace@114..115 " "
                          Type@115..118
                            Ident@115..118 "int"
                        Whitespace@118..119 " "
                        BlockExpr@119..226
                          LCurly@119..120 "{"
                          Whitespace@120..129 "\n        "
                          Module@129..220
                            ModKw@129..132 "mod"
                            Whitespace@132..133 " "
                            Ident@133..145 "third_module"
                            Whitespace@145..146 " "
                            ItemList@146..220
                              LCurly@146..147 "{"
                              Whitespace@147..160 "\n            "
                              FunctionDef@160..210
                                FnKw@160..162 "fn"
                                Whitespace@162..163 " "
                                Ident@163..166 "ccc"
                                ParamList@166..168
                                  LParen@166..167 "("
                                  RParen@167..168 ")"
                                Whitespace@168..169 " "
                                ReturnType@169..175
                                  ThinArrow@169..171 "->"
                                  Whitespace@171..172 " "
                                  Type@172..175
                                    Ident@172..175 "int"
                                Whitespace@175..176 " "
                                BlockExpr@176..210
                                  LCurly@176..177 "{"
                                  Whitespace@177..194 "\n                "
                                  ExprStmt@194..196
                                    Literal@194..196
                                      Integer@194..196 "20"
                                  Whitespace@196..209 "\n            "
                                  RCurly@209..210 "}"
                              Whitespace@210..219 "\n        "
                              RCurly@219..220 "}"
                          Whitespace@220..225 "\n    "
                          RCurly@225..226 "}"
                      Whitespace@226..227 "\n"
                      RCurly@227..228 "}"
                  Whitespace@228..229 "\n"
                  FunctionDef@229..241
                    FnKw@229..231 "fn"
                    Whitespace@231..232 " "
                    Ident@232..236 "main"
                    ParamList@236..238
                      LParen@236..237 "("
                      RParen@237..238 ")"
                    Whitespace@238..239 " "
                    BlockExpr@239..241
                      LCurly@239..240 "{"
                      RCurly@240..241 "}"
                  Whitespace@241..254 "\n            "
            "#]],
        );
    }

    #[test]
    fn parse_module_in_function_and_module() {
        check_debug_tree(
            r#"
fn main() {
    return
}
mod module_aaa {
    mod module_bbb {
        fn function_aaa() -> int {
            mod module_ccc {
                fn function_bbb() -> int {
                    10
                }
            }

            20
        }
    }

    fn function_ccc() -> int {
        30
    }
}
            "#,
            expect![[r#"
                SourceFile@0..321
                  Whitespace@0..1 "\n"
                  FunctionDef@1..25
                    FnKw@1..3 "fn"
                    Whitespace@3..4 " "
                    Ident@4..8 "main"
                    ParamList@8..10
                      LParen@8..9 "("
                      RParen@9..10 ")"
                    Whitespace@10..11 " "
                    BlockExpr@11..25
                      LCurly@11..12 "{"
                      Whitespace@12..17 "\n    "
                      ExprStmt@17..23
                        ReturnExpr@17..23
                          ReturnKw@17..23 "return"
                      Whitespace@23..24 "\n"
                      RCurly@24..25 "}"
                  Whitespace@25..26 "\n"
                  Module@26..308
                    ModKw@26..29 "mod"
                    Whitespace@29..30 " "
                    Ident@30..40 "module_aaa"
                    Whitespace@40..41 " "
                    ItemList@41..308
                      LCurly@41..42 "{"
                      Whitespace@42..47 "\n    "
                      Module@47..257
                        ModKw@47..50 "mod"
                        Whitespace@50..51 " "
                        Ident@51..61 "module_bbb"
                        Whitespace@61..62 " "
                        ItemList@62..257
                          LCurly@62..63 "{"
                          Whitespace@63..72 "\n        "
                          FunctionDef@72..251
                            FnKw@72..74 "fn"
                            Whitespace@74..75 " "
                            Ident@75..87 "function_aaa"
                            ParamList@87..89
                              LParen@87..88 "("
                              RParen@88..89 ")"
                            Whitespace@89..90 " "
                            ReturnType@90..96
                              ThinArrow@90..92 "->"
                              Whitespace@92..93 " "
                              Type@93..96
                                Ident@93..96 "int"
                            Whitespace@96..97 " "
                            BlockExpr@97..251
                              LCurly@97..98 "{"
                              Whitespace@98..111 "\n            "
                              Module@111..225
                                ModKw@111..114 "mod"
                                Whitespace@114..115 " "
                                Ident@115..125 "module_ccc"
                                Whitespace@125..126 " "
                                ItemList@126..225
                                  LCurly@126..127 "{"
                                  Whitespace@127..144 "\n                "
                                  FunctionDef@144..211
                                    FnKw@144..146 "fn"
                                    Whitespace@146..147 " "
                                    Ident@147..159 "function_bbb"
                                    ParamList@159..161
                                      LParen@159..160 "("
                                      RParen@160..161 ")"
                                    Whitespace@161..162 " "
                                    ReturnType@162..168
                                      ThinArrow@162..164 "->"
                                      Whitespace@164..165 " "
                                      Type@165..168
                                        Ident@165..168 "int"
                                    Whitespace@168..169 " "
                                    BlockExpr@169..211
                                      LCurly@169..170 "{"
                                      Whitespace@170..191 "\n                    "
                                      ExprStmt@191..193
                                        Literal@191..193
                                          Integer@191..193 "10"
                                      Whitespace@193..210 "\n                "
                                      RCurly@210..211 "}"
                                  Whitespace@211..224 "\n            "
                                  RCurly@224..225 "}"
                              Whitespace@225..239 "\n\n            "
                              ExprStmt@239..241
                                Literal@239..241
                                  Integer@239..241 "20"
                              Whitespace@241..250 "\n        "
                              RCurly@250..251 "}"
                          Whitespace@251..256 "\n    "
                          RCurly@256..257 "}"
                      Whitespace@257..263 "\n\n    "
                      FunctionDef@263..306
                        FnKw@263..265 "fn"
                        Whitespace@265..266 " "
                        Ident@266..278 "function_ccc"
                        ParamList@278..280
                          LParen@278..279 "("
                          RParen@279..280 ")"
                        Whitespace@280..281 " "
                        ReturnType@281..287
                          ThinArrow@281..283 "->"
                          Whitespace@283..284 " "
                          Type@284..287
                            Ident@284..287 "int"
                        Whitespace@287..288 " "
                        BlockExpr@288..306
                          LCurly@288..289 "{"
                          Whitespace@289..298 "\n        "
                          ExprStmt@298..300
                            Literal@298..300
                              Integer@298..300 "30"
                          Whitespace@300..305 "\n    "
                          RCurly@305..306 "}"
                      Whitespace@306..307 "\n"
                      RCurly@307..308 "}"
                  Whitespace@308..321 "\n            "
            "#]],
        );
    }

    #[test]
    fn parse_file_module() {
        check_debug_tree(
            r#"
mod a;
mod b;
"#,
            expect![[r#"
                SourceFile@0..15
                  Whitespace@0..1 "\n"
                  Module@1..7
                    ModKw@1..4 "mod"
                    Whitespace@4..5 " "
                    Ident@5..6 "a"
                    Semicolon@6..7 ";"
                  Whitespace@7..8 "\n"
                  Module@8..14
                    ModKw@8..11 "mod"
                    Whitespace@11..12 " "
                    Ident@12..13 "b"
                    Semicolon@13..14 ";"
                  Whitespace@14..15 "\n"
            "#]],
        );
    }

    #[test]
    fn parse_use() {
        check_debug_tree(
            r#"
use a;
use b::fn_a;
use c::d::fn_b;
"#,
            expect![[r#"
                SourceFile@0..37
                  Whitespace@0..1 "\n"
                  Use@1..7
                    UseKw@1..4 "use"
                    Whitespace@4..5 " "
                    Path@5..6
                      PathSegment@5..6
                        Ident@5..6 "a"
                    Semicolon@6..7 ";"
                  Whitespace@7..8 "\n"
                  Use@8..20
                    UseKw@8..11 "use"
                    Whitespace@11..12 " "
                    Path@12..19
                      PathSegment@12..13
                        Ident@12..13 "b"
                      Colon2@13..15 "::"
                      PathSegment@15..19
                        Ident@15..19 "fn_a"
                    Semicolon@19..20 ";"
                  Whitespace@20..21 "\n"
                  Use@21..36
                    UseKw@21..24 "use"
                    Whitespace@24..25 " "
                    Path@25..35
                      PathSegment@25..26
                        Ident@25..26 "c"
                      Colon2@26..28 "::"
                      PathSegment@28..29
                        Ident@28..29 "d"
                      Colon2@29..31 "::"
                      PathSegment@31..35
                        Ident@31..35 "fn_b"
                    Semicolon@35..36 ";"
                  Whitespace@36..37 "\n"
            "#]],
        );
    }
}
