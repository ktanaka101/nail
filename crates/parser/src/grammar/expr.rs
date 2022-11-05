use lexer::TokenKind;
use syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

pub(super) fn expr(parser: &mut Parser) -> Option<CompletedMarker> {
    expr_binding_power(parser, 0)
}

fn expr_binding_power(parser: &mut Parser, minimum_binding_power: u8) -> Option<CompletedMarker> {
    let mut lhs = lhs(parser)?;

    loop {
        let op = if parser.at(TokenKind::Plus) {
            BinaryOp::Add
        } else if parser.at(TokenKind::Minus) {
            BinaryOp::Sub
        } else if parser.at(TokenKind::Star) {
            BinaryOp::Mul
        } else if parser.at(TokenKind::Slash) {
            BinaryOp::Div
        } else {
            break;
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            break;
        }

        parser.bump();

        let marker = lhs.precede(parser);
        let parsed_rhs = expr_binding_power(parser, right_binding_power);
        lhs = marker.complete(parser, SyntaxKind::InfixExpr);

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
}

impl BinaryOp {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div => (3, 4),
        }
    }
}

enum PrefixOp {
    Neg,
}

impl PrefixOp {
    fn binding_power(&self) -> ((), u8) {
        match self {
            Self::Neg => ((), 5),
        }
    }
}

fn lhs(parser: &mut Parser) -> Option<CompletedMarker> {
    let cm = if parser.at(TokenKind::IntegerLiteral) || parser.at(TokenKind::CharLiteral(false)) {
        literal(parser)
    } else if parser.at(TokenKind::Ident) {
        variable_ref(parser)
    } else if parser.at(TokenKind::Minus) {
        prefix_expr(parser)
    } else if parser.at(TokenKind::LParen) {
        paren_expr(parser)
    } else {
        parser.error();
        return None;
    };

    Some(cm)
}

fn literal(parser: &mut Parser) -> CompletedMarker {
    assert!(matches!(
        parser.peek(),
        Some(TokenKind::IntegerLiteral | TokenKind::CharLiteral(_))
    ));

    validate_literal(parser);

    let marker = parser.start();
    parser.bump();
    marker.complete(parser, SyntaxKind::Literal)
}

fn validate_literal(parser: &mut Parser) {
    assert!(matches!(
        parser.peek(),
        Some(TokenKind::IntegerLiteral | TokenKind::CharLiteral(_))
    ));

    let current_kind = parser.peek();
    if let Some(TokenKind::CharLiteral(terminated)) = current_kind {
        if !terminated {
            parser.error_in_token(vec![TokenKind::SingleQuote]);
        }
    }
}

fn variable_ref(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::Ident));

    let marker = parser.start();
    parser.bump();
    marker.complete(parser, SyntaxKind::VariableRef)
}

fn prefix_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::Minus));

    let marker = parser.start();

    let op = PrefixOp::Neg;
    let ((), right_binding_power) = op.binding_power();

    parser.bump();

    expr_binding_power(parser, right_binding_power);

    marker.complete(parser, SyntaxKind::PrefixExpr)
}

fn paren_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(TokenKind::LParen));

    let marker = parser.start();
    parser.bump();
    expr_binding_power(parser, 0);
    parser.expect(TokenKind::RParen);

    marker.complete(parser, SyntaxKind::ParenExpr)
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_integer() {
        check(
            "123",
            expect![[r#"
                SourceFile@0..3
                  Literal@0..3
                    IntegerLiteral@0..3 "123""#]],
        );
    }

    #[test]
    fn parse_variable_ref() {
        check(
            "counter",
            expect![[r#"
                SourceFile@0..7
                  VariableRef@0..7
                    Ident@0..7 "counter""#]],
        )
    }

    #[test]
    fn parse_simple_binary_expression() {
        check(
            "1+2",
            expect![[r#"
                SourceFile@0..3
                  InfixExpr@0..3
                    Literal@0..1
                      IntegerLiteral@0..1 "1"
                    Plus@1..2 "+"
                    Literal@2..3
                      IntegerLiteral@2..3 "2""#]],
        );
    }

    #[test]
    fn parse_left_associative_binary_expression() {
        check(
            "1+2+3+4",
            expect![[r#"
                SourceFile@0..7
                  InfixExpr@0..7
                    InfixExpr@0..5
                      InfixExpr@0..3
                        Literal@0..1
                          IntegerLiteral@0..1 "1"
                        Plus@1..2 "+"
                        Literal@2..3
                          IntegerLiteral@2..3 "2"
                      Plus@3..4 "+"
                      Literal@4..5
                        IntegerLiteral@4..5 "3"
                    Plus@5..6 "+"
                    Literal@6..7
                      IntegerLiteral@6..7 "4""#]],
        );
    }

    #[test]
    fn parse_binary_expression_with_mixed_binding_power() {
        check(
            "1+2*3-4",
            expect![[r#"
                SourceFile@0..7
                  InfixExpr@0..7
                    InfixExpr@0..5
                      Literal@0..1
                        IntegerLiteral@0..1 "1"
                      Plus@1..2 "+"
                      InfixExpr@2..5
                        Literal@2..3
                          IntegerLiteral@2..3 "2"
                        Star@3..4 "*"
                        Literal@4..5
                          IntegerLiteral@4..5 "3"
                    Minus@5..6 "-"
                    Literal@6..7
                      IntegerLiteral@6..7 "4""#]],
        );
    }

    #[test]
    fn parse_negation() {
        check(
            "-10",
            expect![[r#"
                SourceFile@0..3
                  PrefixExpr@0..3
                    Minus@0..1 "-"
                    Literal@1..3
                      IntegerLiteral@1..3 "10""#]],
        );
    }

    #[test]
    fn negation_has_higher_binding_power_than_infix_operators() {
        check(
            "-20+20",
            expect![[r#"
                SourceFile@0..6
                  InfixExpr@0..6
                    PrefixExpr@0..3
                      Minus@0..1 "-"
                      Literal@1..3
                        IntegerLiteral@1..3 "20"
                    Plus@3..4 "+"
                    Literal@4..6
                      IntegerLiteral@4..6 "20""#]],
        );
    }

    #[test]
    fn parse_nested_parentheses() {
        check(
            "((((((10))))))",
            expect![[r#"
                SourceFile@0..14
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
                                IntegerLiteral@6..8 "10"
                              RParen@8..9 ")"
                            RParen@9..10 ")"
                          RParen@10..11 ")"
                        RParen@11..12 ")"
                      RParen@12..13 ")"
                    RParen@13..14 ")""#]],
        );
    }

    #[test]
    fn parentheses_affect_precedence() {
        check(
            "5*(2+1)",
            expect![[r#"
                SourceFile@0..7
                  InfixExpr@0..7
                    Literal@0..1
                      IntegerLiteral@0..1 "5"
                    Star@1..2 "*"
                    ParenExpr@2..7
                      LParen@2..3 "("
                      InfixExpr@3..6
                        Literal@3..4
                          IntegerLiteral@3..4 "2"
                        Plus@4..5 "+"
                        Literal@5..6
                          IntegerLiteral@5..6 "1"
                      RParen@6..7 ")""#]],
        );
    }

    #[test]
    fn parse_integer_preceded_by_whitespace() {
        check(
            "   9876",
            expect![[r#"
                SourceFile@0..7
                  Whitespace@0..3 "   "
                  Literal@3..7
                    IntegerLiteral@3..7 "9876""#]],
        );
    }

    #[test]
    fn parse_integer_followed_by_whitespace() {
        check(
            "999   ",
            expect![[r#"
                SourceFile@0..6
                  Literal@0..6
                    IntegerLiteral@0..3 "999"
                    Whitespace@3..6 "   ""#]],
        );
    }

    #[test]
    fn parse_integer_surrounded_by_whitespace() {
        check(
            " 123     ",
            expect![[r#"
                SourceFile@0..9
                  Whitespace@0..1 " "
                  Literal@1..9
                    IntegerLiteral@1..4 "123"
                    Whitespace@4..9 "     ""#]],
        );
    }

    #[test]
    fn parse_char() {
        check(
            "'a'",
            expect![[r#"
                SourceFile@0..3
                  Literal@0..3
                    CharLiteral@0..3 "'a'""#]],
        );
    }

    #[test]
    fn parse_unterminated_char() {
        check(
            "'a",
            expect![[r#"
                SourceFile@0..2
                  Literal@0..2
                    CharLiteral@0..2 "'a"
                error at 0..2: expected ''', in charLiteral"#]],
        );
    }

    #[test]
    fn parse_unterminated_char_with_statement() {
        check(
            "'a let b = 10",
            expect![[r#"
                SourceFile@0..13
                  Literal@0..3
                    CharLiteral@0..2 "'a"
                    Whitespace@2..3 " "
                  VariableDef@3..13
                    LetKw@3..6 "let"
                    Whitespace@6..7 " "
                    Ident@7..8 "b"
                    Whitespace@8..9 " "
                    Eq@9..10 "="
                    Whitespace@10..11 " "
                    Literal@11..13
                      IntegerLiteral@11..13 "10"
                error at 0..2: expected ''', in charLiteral"#]],
        );
    }

    #[test]
    fn parse_binary_expression_with_whitespace() {
        check(
            " 1 +   2* 3 ",
            expect![[r#"
                SourceFile@0..12
                  Whitespace@0..1 " "
                  InfixExpr@1..12
                    Literal@1..3
                      IntegerLiteral@1..2 "1"
                      Whitespace@2..3 " "
                    Plus@3..4 "+"
                    Whitespace@4..7 "   "
                    InfixExpr@7..12
                      Literal@7..8
                        IntegerLiteral@7..8 "2"
                      Star@8..9 "*"
                      Whitespace@9..10 " "
                      Literal@10..12
                        IntegerLiteral@10..11 "3"
                        Whitespace@11..12 " ""#]],
        );
    }

    #[test]
    fn parse_unclosed_parentheses() {
        check(
            "(foo",
            expect![[r#"
                SourceFile@0..4
                  ParenExpr@0..4
                    LParen@0..1 "("
                    VariableRef@1..4
                      Ident@1..4 "foo"
                error at 1..4: expected '+', '-', '*', '/' or ')'"#]],
        );
    }

    #[test]
    fn parse_multi_recover() {
        check(
            "(1+",
            expect![[r#"
                SourceFile@0..3
                  ParenExpr@0..3
                    LParen@0..1 "("
                    InfixExpr@1..3
                      Literal@1..2
                        IntegerLiteral@1..2 "1"
                      Plus@2..3 "+"
                error at 2..3: expected integerLiteral, charLiteral, identifier, '-' or '('
                error at 2..3: expected ')'"#]],
        );
    }
}
