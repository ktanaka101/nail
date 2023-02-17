use lexer::TokenKind;
use syntax::SyntaxKind;

use crate::{
    grammar::stmt::parse_stmt_on_block,
    parser::{marker::CompletedMarker, Parser},
};

pub(super) const EXPR_FIRST: [TokenKind; 13] = [
    TokenKind::StringLiteral,
    TokenKind::CharLiteral(false),
    TokenKind::CharLiteral(true),
    TokenKind::IntegerLiteral,
    TokenKind::TrueKw,
    TokenKind::FalseKw,
    TokenKind::Ident,
    TokenKind::Bang,
    TokenKind::Minus,
    TokenKind::LParen,
    TokenKind::LCurly,
    TokenKind::IfKw,
    TokenKind::ReturnKw,
];

pub(super) const ITEM_FIRST: &[TokenKind] = &[TokenKind::FnKw, TokenKind::ModKw];

pub(super) fn parse_expr(parser: &mut Parser) -> Option<CompletedMarker> {
    parse_expr_binding_power(parser, 0)
}

fn parse_expr_binding_power(
    parser: &mut Parser,
    minimum_binding_power: u8,
) -> Option<CompletedMarker> {
    let mut lhs = parse_lhs(parser)?;

    loop {
        let op = if parser.at(TokenKind::Plus) {
            BinaryOp::Add
        } else if parser.at(TokenKind::Minus) {
            BinaryOp::Sub
        } else if parser.at(TokenKind::Star) {
            BinaryOp::Mul
        } else if parser.at(TokenKind::Slash) {
            BinaryOp::Div
        } else if parser.at(TokenKind::Eq2) {
            BinaryOp::Equal
        } else if parser.at(TokenKind::LAngle) {
            BinaryOp::LessThan
        } else if parser.at(TokenKind::RAngle) {
            BinaryOp::GreaterThan
        } else {
            break;
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            break;
        }

        parser.bump();

        let marker = lhs.precede(parser);
        let parsed_rhs = parse_expr_binding_power(parser, right_binding_power);
        lhs = marker.complete(parser, SyntaxKind::BinaryExpr);

        if parsed_rhs.is_none() {
            break;
        }
    }

    Some(lhs)
}

enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    GreaterThan,
    LessThan,
}

impl BinaryOp {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Equal => (1, 2),
            Self::GreaterThan | Self::LessThan => (3, 4),
            Self::Add | Self::Sub => (5, 6),
            Self::Mul | Self::Div => (7, 8),
        }
    }
}

enum PrefixOp {
    Neg,
    Not,
}

impl PrefixOp {
    fn binding_power(&self) -> ((), u8) {
        match self {
            Self::Not => ((), 9),
            Self::Neg => ((), 10),
        }
    }
}

fn parse_lhs(parser: &mut Parser) -> Option<CompletedMarker> {
    let cm = if parser.at(TokenKind::IntegerLiteral)
        || parser.at(TokenKind::CharLiteral(false))
        || parser.at(TokenKind::StringLiteral)
        || parser.at(TokenKind::TrueKw)
        || parser.at(TokenKind::FalseKw)
    {
        parse_literal(parser)
    } else if parser.at(TokenKind::Ident) {
        parse_path_expr_or_call(parser)
    } else if parser.at_set(&[TokenKind::Minus, TokenKind::Bang]) {
        parse_prefix_expr(parser)
    } else if parser.at(TokenKind::LParen) {
        parse_paren_expr(parser)
    } else if parser.at(TokenKind::LCurly) {
        parse_block(parser)
    } else if parser.at(TokenKind::IfKw) {
        parse_if(parser)
    } else if parser.at(TokenKind::ReturnKw) {
        parse_return(parser)
    } else {
        parser.error_with_recovery_set_only_default_on_block();
        return None;
    };

    Some(cm)
}

fn parse_literal(parser: &mut Parser) -> CompletedMarker {
    assert!(matches!(
        parser.peek(),
        Some(
            TokenKind::IntegerLiteral
                | TokenKind::CharLiteral(_)
                | TokenKind::StringLiteral
                | TokenKind::TrueKw
                | TokenKind::FalseKw
        )
    ));

    validate_literal(parser);

    let marker = parser.start();
    parser.bump();
    marker.complete(parser, SyntaxKind::Literal)
}

fn validate_literal(parser: &mut Parser) {
    assert!(matches!(
        parser.peek(),
        Some(
            TokenKind::IntegerLiteral
                | TokenKind::CharLiteral(_)
                | TokenKind::StringLiteral
                | TokenKind::TrueKw
                | TokenKind::FalseKw
        )
    ));

    let current_kind = parser.peek();
    if let Some(TokenKind::CharLiteral(terminated)) = current_kind {
        if !terminated {
            parser.error_in_token(vec![TokenKind::SingleQuote]);
        }
    }
}

fn parse_path_expr_or_call(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::Ident));

    let marker = parser.start();

    parse_path(parser);

    if parser.peek() == Some(TokenKind::LParen) {
        parse_args(parser);
        marker.complete(parser, SyntaxKind::Call)
    } else {
        marker.complete(parser, SyntaxKind::PathExpr)
    }
}

fn parse_path(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::Ident));

    let marker = parser.start();

    parse_path_segment(parser);
    while parser.at(TokenKind::Colon2) {
        parser.bump();
        parse_path_segment(parser);
    }

    marker.complete(parser, SyntaxKind::Path)
}

fn parse_path_segment(parser: &mut Parser) -> CompletedMarker {
    let marker = parser.start();

    parser.expect_on_block(TokenKind::Ident);

    marker.complete(parser, SyntaxKind::PathSegment)
}

fn parse_args(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::LParen));

    let marker = parser.start();

    parser.bump();
    while parser.at_set_no_expected(&EXPR_FIRST) {
        let marker = parser.start();
        parse_expr(parser);
        marker.complete(parser, SyntaxKind::Arg);
        if parser.at(TokenKind::Comma) {
            parser.bump();
        } else {
            break;
        }
    }
    parser.expect_on_block(TokenKind::RParen);

    marker.complete(parser, SyntaxKind::ArgList)
}

fn parse_prefix_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at_set_no_expected(&[TokenKind::Minus, TokenKind::Bang]));

    let marker = parser.start();

    let op = if parser.at(TokenKind::Minus) {
        PrefixOp::Neg
    } else if parser.at(TokenKind::Bang) {
        PrefixOp::Not
    } else {
        unreachable!();
    };

    let ((), right_binding_power) = op.binding_power();

    parser.bump();

    parse_expr_binding_power(parser, right_binding_power);

    marker.complete(parser, SyntaxKind::UnaryExpr)
}

fn parse_paren_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::LParen));

    let marker = parser.start();
    parser.bump();
    parse_expr_binding_power(parser, 0);
    parser.expect_on_block(TokenKind::RParen);

    marker.complete(parser, SyntaxKind::ParenExpr)
}

fn parse_block(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::LCurly));

    let marker = parser.start();
    parser.bump();
    while !parser.at(TokenKind::RCurly) && !parser.at_end() {
        parse_stmt_on_block(parser);
    }
    parser.expect_on_block(TokenKind::RCurly);

    marker.complete(parser, SyntaxKind::Block)
}

fn parse_if(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::IfKw));

    let marker = parser.start();
    parser.bump();
    parse_expr(parser);

    if parser.at(TokenKind::LCurly) {
        parse_block(parser);
    }

    if parser.at(TokenKind::ElseKw) {
        parser.bump();
        if parser.at(TokenKind::LCurly) {
            parse_block(parser);
        }
    }

    marker.complete(parser, SyntaxKind::IfExpr)
}

fn parse_return(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::ReturnKw));

    let marker = parser.start();
    parser.bump();
    if parser.at_set_no_expected(&EXPR_FIRST) {
        parse_expr(parser);
    }

    marker.complete(parser, SyntaxKind::ReturnExpr)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check_in_block as check;

    #[test]
    fn parse_integer() {
        check(
            "123",
            expect![[r#"
                SourceFile@0..3
                  ExprStmt@0..3
                    Literal@0..3
                      Integer@0..3 "123"
            "#]],
        );
    }

    #[test]
    fn parse_variable_ref() {
        check(
            "counter",
            expect![[r#"
                SourceFile@0..7
                  ExprStmt@0..7
                    PathExpr@0..7
                      Path@0..7
                        PathSegment@0..7
                          Ident@0..7 "counter"
            "#]],
        )
    }

    #[test]
    fn parse_simple_binary_expression() {
        check(
            "1+2",
            expect![[r#"
                SourceFile@0..3
                  ExprStmt@0..3
                    BinaryExpr@0..3
                      Literal@0..1
                        Integer@0..1 "1"
                      Plus@1..2 "+"
                      Literal@2..3
                        Integer@2..3 "2"
            "#]],
        );
    }

    #[test]
    fn parse_binary_expression() {
        check(
            "1 + 2 - 3 * 4 / 5 == 6 < 7 > 8",
            expect![[r#"
                SourceFile@0..30
                  ExprStmt@0..30
                    BinaryExpr@0..30
                      BinaryExpr@0..18
                        BinaryExpr@0..6
                          Literal@0..2
                            Integer@0..1 "1"
                            Whitespace@1..2 " "
                          Plus@2..3 "+"
                          Whitespace@3..4 " "
                          Literal@4..6
                            Integer@4..5 "2"
                            Whitespace@5..6 " "
                        Minus@6..7 "-"
                        Whitespace@7..8 " "
                        BinaryExpr@8..18
                          BinaryExpr@8..14
                            Literal@8..10
                              Integer@8..9 "3"
                              Whitespace@9..10 " "
                            Star@10..11 "*"
                            Whitespace@11..12 " "
                            Literal@12..14
                              Integer@12..13 "4"
                              Whitespace@13..14 " "
                          Slash@14..15 "/"
                          Whitespace@15..16 " "
                          Literal@16..18
                            Integer@16..17 "5"
                            Whitespace@17..18 " "
                      Eq2@18..20 "=="
                      Whitespace@20..21 " "
                      BinaryExpr@21..30
                        BinaryExpr@21..27
                          Literal@21..23
                            Integer@21..22 "6"
                            Whitespace@22..23 " "
                          LAngle@23..24 "<"
                          Whitespace@24..25 " "
                          Literal@25..27
                            Integer@25..26 "7"
                            Whitespace@26..27 " "
                        RAngle@27..28 ">"
                        Whitespace@28..29 " "
                        Literal@29..30
                          Integer@29..30 "8"
            "#]],
        );
    }

    #[test]
    fn parse_left_associative_binary_expression() {
        check(
            "1+2+3+4",
            expect![[r#"
                SourceFile@0..7
                  ExprStmt@0..7
                    BinaryExpr@0..7
                      BinaryExpr@0..5
                        BinaryExpr@0..3
                          Literal@0..1
                            Integer@0..1 "1"
                          Plus@1..2 "+"
                          Literal@2..3
                            Integer@2..3 "2"
                        Plus@3..4 "+"
                        Literal@4..5
                          Integer@4..5 "3"
                      Plus@5..6 "+"
                      Literal@6..7
                        Integer@6..7 "4"
            "#]],
        );
    }

    #[test]
    fn parse_binary_expression_with_mixed_binding_power() {
        check(
            "1+2*3-4",
            expect![[r#"
                SourceFile@0..7
                  ExprStmt@0..7
                    BinaryExpr@0..7
                      BinaryExpr@0..5
                        Literal@0..1
                          Integer@0..1 "1"
                        Plus@1..2 "+"
                        BinaryExpr@2..5
                          Literal@2..3
                            Integer@2..3 "2"
                          Star@3..4 "*"
                          Literal@4..5
                            Integer@4..5 "3"
                      Minus@5..6 "-"
                      Literal@6..7
                        Integer@6..7 "4"
            "#]],
        );
    }

    #[test]
    fn parse_low_precedence_of_equality_operator() {
        check(
            "1 + 2 == 3 * 4",
            expect![[r#"
                SourceFile@0..14
                  ExprStmt@0..14
                    BinaryExpr@0..14
                      BinaryExpr@0..6
                        Literal@0..2
                          Integer@0..1 "1"
                          Whitespace@1..2 " "
                        Plus@2..3 "+"
                        Whitespace@3..4 " "
                        Literal@4..6
                          Integer@4..5 "2"
                          Whitespace@5..6 " "
                      Eq2@6..8 "=="
                      Whitespace@8..9 " "
                      BinaryExpr@9..14
                        Literal@9..11
                          Integer@9..10 "3"
                          Whitespace@10..11 " "
                        Star@11..12 "*"
                        Whitespace@12..13 " "
                        Literal@13..14
                          Integer@13..14 "4"
            "#]],
        );
    }

    #[test]
    fn parse_negation() {
        check(
            "-10",
            expect![[r#"
                SourceFile@0..3
                  ExprStmt@0..3
                    UnaryExpr@0..3
                      Minus@0..1 "-"
                      Literal@1..3
                        Integer@1..3 "10"
            "#]],
        );
    }

    #[test]
    fn parse_not() {
        check(
            "!10",
            expect![[r#"
                SourceFile@0..3
                  ExprStmt@0..3
                    UnaryExpr@0..3
                      Bang@0..1 "!"
                      Literal@1..3
                        Integer@1..3 "10"
            "#]],
        );
    }

    #[test]
    fn negation_has_higher_binding_power_than_infix_operators() {
        check(
            "-20+20",
            expect![[r#"
                SourceFile@0..6
                  ExprStmt@0..6
                    BinaryExpr@0..6
                      UnaryExpr@0..3
                        Minus@0..1 "-"
                        Literal@1..3
                          Integer@1..3 "20"
                      Plus@3..4 "+"
                      Literal@4..6
                        Integer@4..6 "20"
            "#]],
        );
    }

    #[test]
    fn parse_nested_parentheses() {
        check(
            "((((((10))))))",
            expect![[r#"
                SourceFile@0..14
                  ExprStmt@0..14
                    ParenExpr@0..14
                      LParen@0..1 "("
                      ParenExpr@1..13
                        LParen@1..2 "("
                        ParenExpr@2..12
                          LParen@2..3 "("
                          ParenExpr@3..11
                            LParen@3..4 "("
                            ParenExpr@4..10
                              LParen@4..5 "("
                              ParenExpr@5..9
                                LParen@5..6 "("
                                Literal@6..8
                                  Integer@6..8 "10"
                                RParen@8..9 ")"
                              RParen@9..10 ")"
                            RParen@10..11 ")"
                          RParen@11..12 ")"
                        RParen@12..13 ")"
                      RParen@13..14 ")"
            "#]],
        );
    }

    #[test]
    fn parentheses_affect_precedence() {
        check(
            "5*(2+1)",
            expect![[r#"
                SourceFile@0..7
                  ExprStmt@0..7
                    BinaryExpr@0..7
                      Literal@0..1
                        Integer@0..1 "5"
                      Star@1..2 "*"
                      ParenExpr@2..7
                        LParen@2..3 "("
                        BinaryExpr@3..6
                          Literal@3..4
                            Integer@3..4 "2"
                          Plus@4..5 "+"
                          Literal@5..6
                            Integer@5..6 "1"
                        RParen@6..7 ")"
            "#]],
        );
    }

    #[test]
    fn parse_integer_preceded_by_whitespace() {
        check(
            "   9876",
            expect![[r#"
                SourceFile@0..7
                  Whitespace@0..3 "   "
                  ExprStmt@3..7
                    Literal@3..7
                      Integer@3..7 "9876"
            "#]],
        );
    }

    #[test]
    fn parse_integer_followed_by_whitespace() {
        check(
            "999   ",
            expect![[r#"
                SourceFile@0..6
                  ExprStmt@0..6
                    Literal@0..6
                      Integer@0..3 "999"
                      Whitespace@3..6 "   "
            "#]],
        );
    }

    #[test]
    fn parse_integer_surrounded_by_whitespace() {
        check(
            " 123     ",
            expect![[r#"
                SourceFile@0..9
                  Whitespace@0..1 " "
                  ExprStmt@1..9
                    Literal@1..9
                      Integer@1..4 "123"
                      Whitespace@4..9 "     "
            "#]],
        );
    }

    #[test]
    fn parse_char() {
        check(
            "'a'",
            expect![[r#"
                SourceFile@0..3
                  ExprStmt@0..3
                    Literal@0..3
                      Char@0..3 "'a'"
            "#]],
        );
    }

    #[test]
    fn parse_string() {
        check(
            r#""aaa""#,
            expect![[r#"
                SourceFile@0..5
                  ExprStmt@0..5
                    Literal@0..5
                      String@0..5 "\"aaa\""
            "#]],
        );
    }

    #[test]
    fn parse_true() {
        check(
            r#"true"#,
            expect![[r#"
                SourceFile@0..4
                  ExprStmt@0..4
                    Literal@0..4
                      TrueKw@0..4 "true"
            "#]],
        );
    }

    #[test]
    fn parse_false() {
        check(
            r#"false"#,
            expect![[r#"
                SourceFile@0..5
                  ExprStmt@0..5
                    Literal@0..5
                      FalseKw@0..5 "false"
            "#]],
        );
    }

    #[test]
    fn parse_unterminated_char() {
        check(
            "'a",
            expect![[r#"
                SourceFile@0..2
                  ExprStmt@0..2
                    Literal@0..2
                      Char@0..2 "'a"
                error at 0..2: expected ''', in charLiteral
            "#]],
        );
    }

    #[test]
    fn parse_unterminated_char_with_statement() {
        check(
            "'a let b = 10",
            expect![[r#"
                SourceFile@0..13
                  ExprStmt@0..3
                    Literal@0..3
                      Char@0..2 "'a"
                      Whitespace@2..3 " "
                  VariableDef@3..13
                    LetKw@3..6 "let"
                    Whitespace@6..7 " "
                    Ident@7..8 "b"
                    Whitespace@8..9 " "
                    Eq@9..10 "="
                    Whitespace@10..11 " "
                    Literal@11..13
                      Integer@11..13 "10"
                error at 0..2: expected ''', in charLiteral
            "#]],
        );
    }

    #[test]
    fn parse_binary_expression_with_whitespace() {
        check(
            " 1 +   2* 3 ",
            expect![[r#"
                SourceFile@0..12
                  Whitespace@0..1 " "
                  ExprStmt@1..12
                    BinaryExpr@1..12
                      Literal@1..3
                        Integer@1..2 "1"
                        Whitespace@2..3 " "
                      Plus@3..4 "+"
                      Whitespace@4..7 "   "
                      BinaryExpr@7..12
                        Literal@7..8
                          Integer@7..8 "2"
                        Star@8..9 "*"
                        Whitespace@9..10 " "
                        Literal@10..12
                          Integer@10..11 "3"
                          Whitespace@11..12 " "
            "#]],
        );
    }

    #[test]
    fn parse_unclosed_parentheses() {
        check(
            "(foo",
            expect![[r#"
                SourceFile@0..4
                  ExprStmt@0..4
                    ParenExpr@0..4
                      LParen@0..1 "("
                      PathExpr@1..4
                        Path@1..4
                          PathSegment@1..4
                            Ident@1..4 "foo"
                error at 1..4: expected '::', '+', '-', '*', '/', '==', '<', '>' or ')'
            "#]],
        );
    }

    #[test]
    fn parse_multi_recover() {
        check(
            "(1+",
            expect![[r#"
                SourceFile@0..3
                  ExprStmt@0..3
                    ParenExpr@0..3
                      LParen@0..1 "("
                      BinaryExpr@1..3
                        Literal@1..2
                          Integer@1..2 "1"
                        Plus@2..3 "+"
                error at 2..3: expected integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '!', '(', '{', 'if' or 'return'
                error at 2..3: expected ')'
            "#]],
        );
    }

    #[test]
    fn parse_block() {
        check(
            "{ 1 }",
            expect![[r#"
                SourceFile@0..5
                  ExprStmt@0..5
                    Block@0..5
                      LCurly@0..1 "{"
                      Whitespace@1..2 " "
                      ExprStmt@2..4
                        Literal@2..4
                          Integer@2..3 "1"
                          Whitespace@3..4 " "
                      RCurly@4..5 "}"
            "#]],
        );
    }

    #[test]
    fn parse_block_missing_close() {
        check(
            "{ 1 ",
            expect![[r#"
                SourceFile@0..4
                  ExprStmt@0..4
                    Block@0..4
                      LCurly@0..1 "{"
                      Whitespace@1..2 " "
                      ExprStmt@2..4
                        Literal@2..4
                          Integer@2..3 "1"
                          Whitespace@3..4 " "
                error at 3..4: expected '+', '-', '*', '/', '==', '<', '>', ';' or '}'
            "#]],
        );
    }

    #[test]
    fn parse_nested_block() {
        check(
            r#"{
  let a = 10
  { 1 }
}"#,
            expect![[r#"
                SourceFile@0..24
                  ExprStmt@0..24
                    Block@0..24
                      LCurly@0..1 "{"
                      Whitespace@1..4 "\n  "
                      VariableDef@4..17
                        LetKw@4..7 "let"
                        Whitespace@7..8 " "
                        Ident@8..9 "a"
                        Whitespace@9..10 " "
                        Eq@10..11 "="
                        Whitespace@11..12 " "
                        Literal@12..17
                          Integer@12..14 "10"
                          Whitespace@14..17 "\n  "
                      ExprStmt@17..23
                        Block@17..23
                          LCurly@17..18 "{"
                          Whitespace@18..19 " "
                          ExprStmt@19..21
                            Literal@19..21
                              Integer@19..20 "1"
                              Whitespace@20..21 " "
                          RCurly@21..22 "}"
                          Whitespace@22..23 "\n"
                      RCurly@23..24 "}"
            "#]],
        );
    }

    #[test]
    fn parse_call() {
        check(
            "a()",
            expect![[r#"
                SourceFile@0..3
                  ExprStmt@0..3
                    Call@0..3
                      Path@0..1
                        PathSegment@0..1
                          Ident@0..1 "a"
                      ArgList@1..3
                        LParen@1..2 "("
                        RParen@2..3 ")"
            "#]],
        );
    }

    #[test]
    fn parse_call_with_args() {
        check(
            "a(x, y)",
            expect![[r#"
                SourceFile@0..7
                  ExprStmt@0..7
                    Call@0..7
                      Path@0..1
                        PathSegment@0..1
                          Ident@0..1 "a"
                      ArgList@1..7
                        LParen@1..2 "("
                        Arg@2..3
                          PathExpr@2..3
                            Path@2..3
                              PathSegment@2..3
                                Ident@2..3 "x"
                        Comma@3..4 ","
                        Whitespace@4..5 " "
                        Arg@5..6
                          PathExpr@5..6
                            Path@5..6
                              PathSegment@5..6
                                Ident@5..6 "y"
                        RParen@6..7 ")"
            "#]],
        );

        check(
            r#"aaa("aaa", true, 'a', 10)"#,
            expect![[r#"
                SourceFile@0..25
                  ExprStmt@0..25
                    Call@0..25
                      Path@0..3
                        PathSegment@0..3
                          Ident@0..3 "aaa"
                      ArgList@3..25
                        LParen@3..4 "("
                        Arg@4..9
                          Literal@4..9
                            String@4..9 "\"aaa\""
                        Comma@9..10 ","
                        Whitespace@10..11 " "
                        Arg@11..15
                          Literal@11..15
                            TrueKw@11..15 "true"
                        Comma@15..16 ","
                        Whitespace@16..17 " "
                        Arg@17..20
                          Literal@17..20
                            Char@17..20 "'a'"
                        Comma@20..21 ","
                        Whitespace@21..22 " "
                        Arg@22..24
                          Literal@22..24
                            Integer@22..24 "10"
                        RParen@24..25 ")"
            "#]],
        );
    }

    #[test]
    fn parse_call_missing_rparen() {
        check(
            "a(x, y",
            expect![[r#"
                SourceFile@0..6
                  ExprStmt@0..6
                    Call@0..6
                      Path@0..1
                        PathSegment@0..1
                          Ident@0..1 "a"
                      ArgList@1..6
                        LParen@1..2 "("
                        Arg@2..3
                          PathExpr@2..3
                            Path@2..3
                              PathSegment@2..3
                                Ident@2..3 "x"
                        Comma@3..4 ","
                        Whitespace@4..5 " "
                        Arg@5..6
                          PathExpr@5..6
                            Path@5..6
                              PathSegment@5..6
                                Ident@5..6 "y"
                error at 5..6: expected '::', '+', '-', '*', '/', '==', '<', '>', ',' or ')'
            "#]],
        );

        check(
            "a(x,",
            expect![[r#"
                SourceFile@0..4
                  ExprStmt@0..4
                    Call@0..4
                      Path@0..1
                        PathSegment@0..1
                          Ident@0..1 "a"
                      ArgList@1..4
                        LParen@1..2 "("
                        Arg@2..3
                          PathExpr@2..3
                            Path@2..3
                              PathSegment@2..3
                                Ident@2..3 "x"
                        Comma@3..4 ","
                error at 3..4: expected ')'
            "#]],
        );

        check(
            "a(x",
            expect![[r#"
                SourceFile@0..3
                  ExprStmt@0..3
                    Call@0..3
                      Path@0..1
                        PathSegment@0..1
                          Ident@0..1 "a"
                      ArgList@1..3
                        LParen@1..2 "("
                        Arg@2..3
                          PathExpr@2..3
                            Path@2..3
                              PathSegment@2..3
                                Ident@2..3 "x"
                error at 2..3: expected '::', '+', '-', '*', '/', '==', '<', '>', ',' or ')'
            "#]],
        );

        check(
            "a(",
            expect![[r#"
                SourceFile@0..2
                  ExprStmt@0..2
                    Call@0..2
                      Path@0..1
                        PathSegment@0..1
                          Ident@0..1 "a"
                      ArgList@1..2
                        LParen@1..2 "("
                error at 1..2: expected ')'
            "#]],
        );
    }

    #[test]
    fn parse_call_missing_expr() {
        check(
            "a(x,)",
            expect![[r#"
                SourceFile@0..5
                  ExprStmt@0..5
                    Call@0..5
                      Path@0..1
                        PathSegment@0..1
                          Ident@0..1 "a"
                      ArgList@1..5
                        LParen@1..2 "("
                        Arg@2..3
                          PathExpr@2..3
                            Path@2..3
                              PathSegment@2..3
                                Ident@2..3 "x"
                        Comma@3..4 ","
                        RParen@4..5 ")"
            "#]],
        );
    }

    #[test]
    fn parse_call_missing_comma() {
        check(
            "a(x y)",
            expect![[r#"
                SourceFile@0..6
                  ExprStmt@0..5
                    Call@0..5
                      Path@0..1
                        PathSegment@0..1
                          Ident@0..1 "a"
                      ArgList@1..5
                        LParen@1..2 "("
                        Arg@2..4
                          PathExpr@2..4
                            Path@2..4
                              PathSegment@2..4
                                Ident@2..3 "x"
                                Whitespace@3..4 " "
                        Error@4..5
                          Ident@4..5 "y"
                  ExprStmt@5..6
                    Error@5..6
                      RParen@5..6 ")"
                error at 4..5: expected '::', '+', '-', '*', '/', '==', '<', '>', ',' or ')', but found identifier
                error at 5..6: expected '+', '-', '*', '/', '==', '<', '>', ';', 'let', 'fn', 'mod', integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '!', '(', '{', 'if' or 'return', but found ')'
            "#]],
        );
    }

    #[test]
    fn parse_expr_on_arg() {
        check(
            "a(x + y)",
            expect![[r#"
                SourceFile@0..8
                  ExprStmt@0..8
                    Call@0..8
                      Path@0..1
                        PathSegment@0..1
                          Ident@0..1 "a"
                      ArgList@1..8
                        LParen@1..2 "("
                        Arg@2..7
                          BinaryExpr@2..7
                            PathExpr@2..4
                              Path@2..4
                                PathSegment@2..4
                                  Ident@2..3 "x"
                                  Whitespace@3..4 " "
                            Plus@4..5 "+"
                            Whitespace@5..6 " "
                            PathExpr@6..7
                              Path@6..7
                                PathSegment@6..7
                                  Ident@6..7 "y"
                        RParen@7..8 ")"
            "#]],
        );

        check(
            "a({ x + y })",
            expect![[r#"
                SourceFile@0..12
                  ExprStmt@0..12
                    Call@0..12
                      Path@0..1
                        PathSegment@0..1
                          Ident@0..1 "a"
                      ArgList@1..12
                        LParen@1..2 "("
                        Arg@2..11
                          Block@2..11
                            LCurly@2..3 "{"
                            Whitespace@3..4 " "
                            ExprStmt@4..10
                              BinaryExpr@4..10
                                PathExpr@4..6
                                  Path@4..6
                                    PathSegment@4..6
                                      Ident@4..5 "x"
                                      Whitespace@5..6 " "
                                Plus@6..7 "+"
                                Whitespace@7..8 " "
                                PathExpr@8..10
                                  Path@8..10
                                    PathSegment@8..10
                                      Ident@8..9 "y"
                                      Whitespace@9..10 " "
                            RCurly@10..11 "}"
                        RParen@11..12 ")"
            "#]],
        );
    }

    #[test]
    fn parse_if_expr() {
        check(
            "if true { 10 }",
            expect![[r#"
                SourceFile@0..14
                  ExprStmt@0..14
                    IfExpr@0..14
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      Literal@3..8
                        TrueKw@3..7 "true"
                        Whitespace@7..8 " "
                      Block@8..14
                        LCurly@8..9 "{"
                        Whitespace@9..10 " "
                        ExprStmt@10..13
                          Literal@10..13
                            Integer@10..12 "10"
                            Whitespace@12..13 " "
                        RCurly@13..14 "}"
            "#]],
        );

        check(
            "if true { 10 } else { 20 }",
            expect![[r#"
                SourceFile@0..26
                  ExprStmt@0..26
                    IfExpr@0..26
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      Literal@3..8
                        TrueKw@3..7 "true"
                        Whitespace@7..8 " "
                      Block@8..15
                        LCurly@8..9 "{"
                        Whitespace@9..10 " "
                        ExprStmt@10..13
                          Literal@10..13
                            Integer@10..12 "10"
                            Whitespace@12..13 " "
                        RCurly@13..14 "}"
                        Whitespace@14..15 " "
                      ElseKw@15..19 "else"
                      Whitespace@19..20 " "
                      Block@20..26
                        LCurly@20..21 "{"
                        Whitespace@21..22 " "
                        ExprStmt@22..25
                          Literal@22..25
                            Integer@22..24 "20"
                            Whitespace@24..25 " "
                        RCurly@25..26 "}"
            "#]],
        )
    }

    #[test]
    fn parse_if_is_expr() {
        check(
            "let a = if true { 10 } else { 20 }",
            expect![[r#"
                SourceFile@0..34
                  VariableDef@0..34
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Eq@6..7 "="
                    Whitespace@7..8 " "
                    IfExpr@8..34
                      IfKw@8..10 "if"
                      Whitespace@10..11 " "
                      Literal@11..16
                        TrueKw@11..15 "true"
                        Whitespace@15..16 " "
                      Block@16..23
                        LCurly@16..17 "{"
                        Whitespace@17..18 " "
                        ExprStmt@18..21
                          Literal@18..21
                            Integer@18..20 "10"
                            Whitespace@20..21 " "
                        RCurly@21..22 "}"
                        Whitespace@22..23 " "
                      ElseKw@23..27 "else"
                      Whitespace@27..28 " "
                      Block@28..34
                        LCurly@28..29 "{"
                        Whitespace@29..30 " "
                        ExprStmt@30..33
                          Literal@30..33
                            Integer@30..32 "20"
                            Whitespace@32..33 " "
                        RCurly@33..34 "}"
            "#]],
        )
    }

    #[test]
    fn parse_if_expr_missing_condition() {
        check(
            "if { 10 } else { 20 }",
            expect![[r#"
                SourceFile@0..21
                  ExprStmt@0..21
                    IfExpr@0..21
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      Block@3..10
                        LCurly@3..4 "{"
                        Whitespace@4..5 " "
                        ExprStmt@5..8
                          Literal@5..8
                            Integer@5..7 "10"
                            Whitespace@7..8 " "
                        RCurly@8..9 "}"
                        Whitespace@9..10 " "
                      ElseKw@10..14 "else"
                      Whitespace@14..15 " "
                      Block@15..21
                        LCurly@15..16 "{"
                        Whitespace@16..17 " "
                        ExprStmt@17..20
                          Literal@17..20
                            Integer@17..19 "20"
                            Whitespace@19..20 " "
                        RCurly@20..21 "}"
            "#]],
        );
    }

    #[test]
    fn parse_if_expr_missing_then_block() {
        check(
            "if true else { 20 }",
            expect![[r#"
                SourceFile@0..19
                  ExprStmt@0..19
                    IfExpr@0..19
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      Literal@3..8
                        TrueKw@3..7 "true"
                        Whitespace@7..8 " "
                      ElseKw@8..12 "else"
                      Whitespace@12..13 " "
                      Block@13..19
                        LCurly@13..14 "{"
                        Whitespace@14..15 " "
                        ExprStmt@15..18
                          Literal@15..18
                            Integer@15..17 "20"
                            Whitespace@17..18 " "
                        RCurly@18..19 "}"
            "#]],
        );
    }

    #[test]
    fn parse_if_expr_missing_else() {
        check(
            "if true { 10 } { 20 }",
            expect![[r#"
                SourceFile@0..21
                  ExprStmt@0..15
                    IfExpr@0..15
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      Literal@3..8
                        TrueKw@3..7 "true"
                        Whitespace@7..8 " "
                      Block@8..15
                        LCurly@8..9 "{"
                        Whitespace@9..10 " "
                        ExprStmt@10..13
                          Literal@10..13
                            Integer@10..12 "10"
                            Whitespace@12..13 " "
                        RCurly@13..14 "}"
                        Whitespace@14..15 " "
                  ExprStmt@15..21
                    Block@15..21
                      LCurly@15..16 "{"
                      Whitespace@16..17 " "
                      ExprStmt@17..20
                        Literal@17..20
                          Integer@17..19 "20"
                          Whitespace@19..20 " "
                      RCurly@20..21 "}"
            "#]],
        );
    }

    #[test]
    fn parse_if_expr_missing_else_block() {
        check(
            "if true { 10 } else",
            expect![[r#"
                SourceFile@0..19
                  ExprStmt@0..19
                    IfExpr@0..19
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      Literal@3..8
                        TrueKw@3..7 "true"
                        Whitespace@7..8 " "
                      Block@8..15
                        LCurly@8..9 "{"
                        Whitespace@9..10 " "
                        ExprStmt@10..13
                          Literal@10..13
                            Integer@10..12 "10"
                            Whitespace@12..13 " "
                        RCurly@13..14 "}"
                        Whitespace@14..15 " "
                      ElseKw@15..19 "else"
            "#]],
        );
    }

    #[test]
    fn parse_if_block_condition() {
        check(
            "if { true } { 10 } else { 20 }",
            expect![[r#"
                SourceFile@0..30
                  ExprStmt@0..30
                    IfExpr@0..30
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      Block@3..12
                        LCurly@3..4 "{"
                        Whitespace@4..5 " "
                        ExprStmt@5..10
                          Literal@5..10
                            TrueKw@5..9 "true"
                            Whitespace@9..10 " "
                        RCurly@10..11 "}"
                        Whitespace@11..12 " "
                      Block@12..19
                        LCurly@12..13 "{"
                        Whitespace@13..14 " "
                        ExprStmt@14..17
                          Literal@14..17
                            Integer@14..16 "10"
                            Whitespace@16..17 " "
                        RCurly@17..18 "}"
                        Whitespace@18..19 " "
                      ElseKw@19..23 "else"
                      Whitespace@23..24 " "
                      Block@24..30
                        LCurly@24..25 "{"
                        Whitespace@25..26 " "
                        ExprStmt@26..29
                          Literal@26..29
                            Integer@26..28 "20"
                            Whitespace@28..29 " "
                        RCurly@29..30 "}"
            "#]],
        );
    }

    #[test]
    fn parse_equal() {
        check(
            "10 == 20",
            expect![[r#"
                SourceFile@0..8
                  ExprStmt@0..8
                    BinaryExpr@0..8
                      Literal@0..3
                        Integer@0..2 "10"
                        Whitespace@2..3 " "
                      Eq2@3..5 "=="
                      Whitespace@5..6 " "
                      Literal@6..8
                        Integer@6..8 "20"
            "#]],
        );
    }

    #[test]
    fn parse_return() {
        check(
            "return",
            expect![[r#"
                SourceFile@0..6
                  ExprStmt@0..6
                    ReturnExpr@0..6
                      ReturnKw@0..6 "return"
            "#]],
        );
    }

    #[test]
    fn parse_return_with_value() {
        check(
            "return 10",
            expect![[r#"
                SourceFile@0..9
                  ExprStmt@0..9
                    ReturnExpr@0..9
                      ReturnKw@0..6 "return"
                      Whitespace@6..7 " "
                      Literal@7..9
                        Integer@7..9 "10"
            "#]],
        );
    }

    #[test]
    fn support_return_values() {
        check(
            r#"
                return 10
                return 'a'
                return "aaa"
                return true
                return false
                return -10
                return (10)
                return (10 + 20)
                return a
                return if true { 10 } else { 20 }
                return { 10 }
                return return 10
            "#,
            expect![[r#"
                SourceFile@0..378
                  Whitespace@0..17 "\n                "
                  ExprStmt@17..43
                    ReturnExpr@17..43
                      ReturnKw@17..23 "return"
                      Whitespace@23..24 " "
                      Literal@24..43
                        Integer@24..26 "10"
                        Whitespace@26..43 "\n                "
                  ExprStmt@43..70
                    ReturnExpr@43..70
                      ReturnKw@43..49 "return"
                      Whitespace@49..50 " "
                      Literal@50..70
                        Char@50..53 "'a'"
                        Whitespace@53..70 "\n                "
                  ExprStmt@70..99
                    ReturnExpr@70..99
                      ReturnKw@70..76 "return"
                      Whitespace@76..77 " "
                      Literal@77..99
                        String@77..82 "\"aaa\""
                        Whitespace@82..99 "\n                "
                  ExprStmt@99..127
                    ReturnExpr@99..127
                      ReturnKw@99..105 "return"
                      Whitespace@105..106 " "
                      Literal@106..127
                        TrueKw@106..110 "true"
                        Whitespace@110..127 "\n                "
                  ExprStmt@127..156
                    ReturnExpr@127..156
                      ReturnKw@127..133 "return"
                      Whitespace@133..134 " "
                      Literal@134..156
                        FalseKw@134..139 "false"
                        Whitespace@139..156 "\n                "
                  ExprStmt@156..183
                    ReturnExpr@156..183
                      ReturnKw@156..162 "return"
                      Whitespace@162..163 " "
                      UnaryExpr@163..183
                        Minus@163..164 "-"
                        Literal@164..183
                          Integer@164..166 "10"
                          Whitespace@166..183 "\n                "
                  ExprStmt@183..211
                    ReturnExpr@183..211
                      ReturnKw@183..189 "return"
                      Whitespace@189..190 " "
                      ParenExpr@190..211
                        LParen@190..191 "("
                        Literal@191..193
                          Integer@191..193 "10"
                        RParen@193..194 ")"
                        Whitespace@194..211 "\n                "
                  ExprStmt@211..244
                    ReturnExpr@211..244
                      ReturnKw@211..217 "return"
                      Whitespace@217..218 " "
                      ParenExpr@218..244
                        LParen@218..219 "("
                        BinaryExpr@219..226
                          Literal@219..222
                            Integer@219..221 "10"
                            Whitespace@221..222 " "
                          Plus@222..223 "+"
                          Whitespace@223..224 " "
                          Literal@224..226
                            Integer@224..226 "20"
                        RParen@226..227 ")"
                        Whitespace@227..244 "\n                "
                  ExprStmt@244..269
                    ReturnExpr@244..269
                      ReturnKw@244..250 "return"
                      Whitespace@250..251 " "
                      PathExpr@251..269
                        Path@251..269
                          PathSegment@251..269
                            Ident@251..252 "a"
                            Whitespace@252..269 "\n                "
                  ExprStmt@269..319
                    ReturnExpr@269..319
                      ReturnKw@269..275 "return"
                      Whitespace@275..276 " "
                      IfExpr@276..319
                        IfKw@276..278 "if"
                        Whitespace@278..279 " "
                        Literal@279..284
                          TrueKw@279..283 "true"
                          Whitespace@283..284 " "
                        Block@284..291
                          LCurly@284..285 "{"
                          Whitespace@285..286 " "
                          ExprStmt@286..289
                            Literal@286..289
                              Integer@286..288 "10"
                              Whitespace@288..289 " "
                          RCurly@289..290 "}"
                          Whitespace@290..291 " "
                        ElseKw@291..295 "else"
                        Whitespace@295..296 " "
                        Block@296..319
                          LCurly@296..297 "{"
                          Whitespace@297..298 " "
                          ExprStmt@298..301
                            Literal@298..301
                              Integer@298..300 "20"
                              Whitespace@300..301 " "
                          RCurly@301..302 "}"
                          Whitespace@302..319 "\n                "
                  ExprStmt@319..349
                    ReturnExpr@319..349
                      ReturnKw@319..325 "return"
                      Whitespace@325..326 " "
                      Block@326..349
                        LCurly@326..327 "{"
                        Whitespace@327..328 " "
                        ExprStmt@328..331
                          Literal@328..331
                            Integer@328..330 "10"
                            Whitespace@330..331 " "
                        RCurly@331..332 "}"
                        Whitespace@332..349 "\n                "
                  ExprStmt@349..378
                    ReturnExpr@349..378
                      ReturnKw@349..355 "return"
                      Whitespace@355..356 " "
                      ReturnExpr@356..378
                        ReturnKw@356..362 "return"
                        Whitespace@362..363 " "
                        Literal@363..378
                          Integer@363..365 "10"
                          Whitespace@365..378 "\n            "
            "#]],
        );
    }

    #[test]
    fn parse_path() {
        check(
            "a::b::c",
            expect![[r#"
                SourceFile@0..7
                  ExprStmt@0..7
                    PathExpr@0..7
                      Path@0..7
                        PathSegment@0..1
                          Ident@0..1 "a"
                        Colon2@1..3 "::"
                        PathSegment@3..4
                          Ident@3..4 "b"
                        Colon2@4..6 "::"
                        PathSegment@6..7
                          Ident@6..7 "c"
            "#]],
        );
    }

    #[test]
    fn parse_path_call() {
        check(
            "a::b::c()",
            expect![[r#"
                SourceFile@0..9
                  ExprStmt@0..9
                    Call@0..9
                      Path@0..7
                        PathSegment@0..1
                          Ident@0..1 "a"
                        Colon2@1..3 "::"
                        PathSegment@3..4
                          Ident@3..4 "b"
                        Colon2@4..6 "::"
                        PathSegment@6..7
                          Ident@6..7 "c"
                      ArgList@7..9
                        LParen@7..8 "("
                        RParen@8..9 ")"
            "#]],
        );
    }

    #[test]
    fn parse_path_call_with_params() {
        check(
            "a::b::c(e, f, g)",
            expect![[r#"
                SourceFile@0..16
                  ExprStmt@0..16
                    Call@0..16
                      Path@0..7
                        PathSegment@0..1
                          Ident@0..1 "a"
                        Colon2@1..3 "::"
                        PathSegment@3..4
                          Ident@3..4 "b"
                        Colon2@4..6 "::"
                        PathSegment@6..7
                          Ident@6..7 "c"
                      ArgList@7..16
                        LParen@7..8 "("
                        Arg@8..9
                          PathExpr@8..9
                            Path@8..9
                              PathSegment@8..9
                                Ident@8..9 "e"
                        Comma@9..10 ","
                        Whitespace@10..11 " "
                        Arg@11..12
                          PathExpr@11..12
                            Path@11..12
                              PathSegment@11..12
                                Ident@11..12 "f"
                        Comma@12..13 ","
                        Whitespace@13..14 " "
                        Arg@14..15
                          PathExpr@14..15
                            Path@14..15
                              PathSegment@14..15
                                Ident@14..15 "g"
                        RParen@15..16 ")"
            "#]],
        );
    }
}
