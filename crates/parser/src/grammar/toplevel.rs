use lexer::TokenKind;
use syntax::SyntaxKind;

use super::stmt;
use crate::parser::{marker::CompletedMarker, Parser, TOPLEVEL_RECOVERY_SET};

pub(super) fn parse_stmt_on_toplevel(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(TokenKind::FnKw) {
        Some(stmt::parse_function_def(parser, &TOPLEVEL_RECOVERY_SET))
    } else if parser.at(TokenKind::ModKw) {
        Some(parse_module(parser, &TOPLEVEL_RECOVERY_SET))
    } else {
        parser.error_with_recovery_set_only_default_on_toplevel();
        None
    }
}

pub(super) fn parse_module(parser: &mut Parser, recovery_set: &[TokenKind]) -> CompletedMarker {
    assert!(parser.at(TokenKind::ModKw));

    let marker = parser.start();
    parser.bump();

    parser.expect_with_recovery_set_no_default(TokenKind::Ident, recovery_set);

    {
        let marker = parser.start();
        if parser.at(TokenKind::LCurly) {
            parser.bump();
            parse_stmt_on_toplevel(parser);
        }

        if parser.at(TokenKind::RCurly) {
            parser.bump();
        }

        marker.complete(parser, SyntaxKind::ItemList);
    }

    marker.complete(parser, SyntaxKind::Module)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check;

    #[test]
    fn parse_variable_definition() {
        check(
            "let foo = bar",
            expect![[r#"
                SourceFile@0..13
                  Error@0..4
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                  Error@4..8
                    Ident@4..7 "foo"
                    Whitespace@7..8 " "
                  Error@8..10
                    Eq@8..9 "="
                    Whitespace@9..10 " "
                  Error@10..13
                    Ident@10..13 "bar"
                error at 0..3: expected 'fn' or 'mod', but found 'let'
                error at 4..7: expected 'fn' or 'mod', but found identifier
                error at 8..9: expected 'fn' or 'mod', but found '='
                error at 10..13: expected 'fn' or 'mod', but found identifier
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
                    Block@16..18
                      LCurly@16..17 "{"
                      RCurly@17..18 "}"
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
                    Block@16..27
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
                    Block@24..30
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
                    Block@22..28
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
                    Block@21..27
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
                    Block@17..23
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
                    Block@16..22
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
                    Block@15..21
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
                    Block@17..23
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
                    Block@7..13
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
                    Block@10..16
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
                  Error@7..11
                    Ident@7..10 "int"
                    Whitespace@10..11 " "
                  Error@11..13
                    LCurly@11..12 "{"
                    Whitespace@12..13 " "
                  Error@13..16
                    Integer@13..15 "10"
                    Whitespace@15..16 " "
                  Error@16..17
                    RCurly@16..17 "}"
                error at 7..10: expected ->, '{', 'fn' or 'mod', but found identifier
                error at 11..12: expected 'fn' or 'mod', but found '{'
                error at 13..15: expected 'fn' or 'mod', but found integerLiteral
                error at 16..17: expected 'fn' or 'mod', but found '}'
            "#]],
        );
    }

    #[test]
    fn parse_module() {
        check(
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
                  Module@1..59
                    ModKw@1..4 "mod"
                    Whitespace@4..5 " "
                    Ident@5..15 "one_module"
                    Whitespace@15..16 " "
                    ItemList@16..59
                      LCurly@16..17 "{"
                      Whitespace@17..22 "\n    "
                      FunctionDef@22..57
                        FnKw@22..24 "fn"
                        Whitespace@24..25 " "
                        Ident@25..28 "bbb"
                        ParamList@28..31
                          LParen@28..29 "("
                          RParen@29..30 ")"
                          Whitespace@30..31 " "
                        ReturnType@31..38
                          ThinArrow@31..33 "->"
                          Whitespace@33..34 " "
                          Type@34..38
                            Ident@34..37 "int"
                            Whitespace@37..38 " "
                        Block@38..57
                          LCurly@38..39 "{"
                          Whitespace@39..48 "\n        "
                          ExprStmt@48..55
                            Literal@48..55
                              Integer@48..50 "10"
                              Whitespace@50..55 "\n    "
                          RCurly@55..56 "}"
                          Whitespace@56..57 "\n"
                      RCurly@57..58 "}"
                      Whitespace@58..59 "\n"
                  FunctionDef@59..84
                    FnKw@59..61 "fn"
                    Whitespace@61..62 " "
                    Ident@62..66 "main"
                    ParamList@66..69
                      LParen@66..67 "("
                      RParen@67..68 ")"
                      Whitespace@68..69 " "
                    Block@69..84
                      LCurly@69..70 "{"
                      RCurly@70..71 "}"
                      Whitespace@71..84 "\n            "
            "#]],
        );
    }

    #[test]
    fn parse_module_in_block() {
        check(
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
                  Module@1..103
                    ModKw@1..4 "mod"
                    Whitespace@4..5 " "
                    Ident@5..15 "one_module"
                    Whitespace@15..16 " "
                    ItemList@16..103
                      LCurly@16..17 "{"
                      Whitespace@17..22 "\n    "
                      Module@22..103
                        ModKw@22..25 "mod"
                        Whitespace@25..26 " "
                        Ident@26..39 "second_module"
                        Whitespace@39..40 " "
                        ItemList@40..103
                          LCurly@40..41 "{"
                          Whitespace@41..50 "\n        "
                          FunctionDef@50..97
                            FnKw@50..52 "fn"
                            Whitespace@52..53 " "
                            Ident@53..56 "aaa"
                            ParamList@56..59
                              LParen@56..57 "("
                              RParen@57..58 ")"
                              Whitespace@58..59 " "
                            ReturnType@59..66
                              ThinArrow@59..61 "->"
                              Whitespace@61..62 " "
                              Type@62..66
                                Ident@62..65 "int"
                                Whitespace@65..66 " "
                            Block@66..97
                              LCurly@66..67 "{"
                              Whitespace@67..80 "\n            "
                              ExprStmt@80..91
                                Literal@80..91
                                  Integer@80..82 "10"
                                  Whitespace@82..91 "\n        "
                              RCurly@91..92 "}"
                              Whitespace@92..97 "\n    "
                          RCurly@97..98 "}"
                          Whitespace@98..103 "\n    "
                  FunctionDef@103..227
                    FnKw@103..105 "fn"
                    Whitespace@105..106 " "
                    Ident@106..109 "bbb"
                    ParamList@109..112
                      LParen@109..110 "("
                      RParen@110..111 ")"
                      Whitespace@111..112 " "
                    ReturnType@112..119
                      ThinArrow@112..114 "->"
                      Whitespace@114..115 " "
                      Type@115..119
                        Ident@115..118 "int"
                        Whitespace@118..119 " "
                    Block@119..227
                      LCurly@119..120 "{"
                      Whitespace@120..129 "\n        "
                      Module@129..225
                        ModKw@129..132 "mod"
                        Whitespace@132..133 " "
                        Ident@133..145 "third_module"
                        Whitespace@145..146 " "
                        ItemList@146..225
                          LCurly@146..147 "{"
                          Whitespace@147..160 "\n            "
                          FunctionDef@160..219
                            FnKw@160..162 "fn"
                            Whitespace@162..163 " "
                            Ident@163..166 "ccc"
                            ParamList@166..169
                              LParen@166..167 "("
                              RParen@167..168 ")"
                              Whitespace@168..169 " "
                            ReturnType@169..176
                              ThinArrow@169..171 "->"
                              Whitespace@171..172 " "
                              Type@172..176
                                Ident@172..175 "int"
                                Whitespace@175..176 " "
                            Block@176..219
                              LCurly@176..177 "{"
                              Whitespace@177..194 "\n                "
                              ExprStmt@194..209
                                Literal@194..209
                                  Integer@194..196 "20"
                                  Whitespace@196..209 "\n            "
                              RCurly@209..210 "}"
                              Whitespace@210..219 "\n        "
                          RCurly@219..220 "}"
                          Whitespace@220..225 "\n    "
                      RCurly@225..226 "}"
                      Whitespace@226..227 "\n"
                  Error@227..229
                    RCurly@227..228 "}"
                    Whitespace@228..229 "\n"
                  FunctionDef@229..254
                    FnKw@229..231 "fn"
                    Whitespace@231..232 " "
                    Ident@232..236 "main"
                    ParamList@236..239
                      LParen@236..237 "("
                      RParen@237..238 ")"
                      Whitespace@238..239 " "
                    Block@239..254
                      LCurly@239..240 "{"
                      RCurly@240..241 "}"
                      Whitespace@241..254 "\n            "
                error at 227..228: expected 'fn' or 'mod', but found '}'
            "#]],
        );
    }
}
