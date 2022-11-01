use super::*;

pub(super) fn expr(parser: &mut Parser) {
    expr_binding_power(parser, 0);
}

fn expr_binding_power(parser: &mut Parser, minimum_binding_power: u8) {
    let mut lhs = if let Some(lhs) = lhs(parser) {
        lhs
    } else {
        return;
    };

    loop {
        let op = match parser.peek() {
            Some(SyntaxKind::Plus) => BinaryOp::Add,
            Some(SyntaxKind::Minus) => BinaryOp::Sub,
            Some(SyntaxKind::Asterisk) => BinaryOp::Mul,
            Some(SyntaxKind::Slash) => BinaryOp::Div,
            _ => return,
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            return;
        }

        parser.bump();

        let marker = lhs.precede(parser);
        expr_binding_power(parser, right_binding_power);
        lhs = marker.complete(parser, SyntaxKind::InfixExpr);
    }
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
    let cm = match parser.peek() {
        Some(SyntaxKind::IntegerLiteral) => literal(parser),
        Some(SyntaxKind::Ident) => variable_ref(parser),
        Some(SyntaxKind::Minus) => prefix_expr(parser),
        Some(SyntaxKind::LParen) => paren_expr(parser),
        _ => return None,
    };

    Some(cm)
}

fn literal(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::IntegerLiteral));

    let marker = parser.start();
    parser.bump();
    marker.complete(parser, SyntaxKind::Literal)
}

fn variable_ref(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::Ident));

    let marker = parser.start();
    parser.bump();
    marker.complete(parser, SyntaxKind::VariableRef)
}

fn prefix_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::Minus));

    let marker = parser.start();

    let op = PrefixOp::Neg;
    let ((), right_binding_power) = op.binding_power();

    parser.bump();

    expr_binding_power(parser, right_binding_power);

    marker.complete(parser, SyntaxKind::PrefixExpr)
}

fn paren_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::LParen));

    let marker = parser.start();

    parser.bump();
    expr_binding_power(parser, 0);

    assert!(parser.at(SyntaxKind::RParen));
    parser.bump();

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
                Root@0..3
                  Literal@0..3
                    IntegerLiteral@0..3 "123""#]],
        );
    }

    #[test]
    fn parse_variable_ref() {
        check(
            "counter",
            expect![[r#"
                Root@0..7
                  VariableRef@0..7
                    Ident@0..7 "counter""#]],
        )
    }

    #[test]
    fn parse_simple_binary_expression() {
        check(
            "1+2",
            expect![[r#"
                Root@0..3
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
                Root@0..7
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
                Root@0..7
                  InfixExpr@0..7
                    InfixExpr@0..5
                      Literal@0..1
                        IntegerLiteral@0..1 "1"
                      Plus@1..2 "+"
                      InfixExpr@2..5
                        Literal@2..3
                          IntegerLiteral@2..3 "2"
                        Asterisk@3..4 "*"
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
                Root@0..3
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
                Root@0..6
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
                Root@0..14
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
                Root@0..7
                  InfixExpr@0..7
                    Literal@0..1
                      IntegerLiteral@0..1 "5"
                    Asterisk@1..2 "*"
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
                Root@0..7
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
                Root@0..6
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
                Root@0..9
                  Whitespace@0..1 " "
                  Literal@1..9
                    IntegerLiteral@1..4 "123"
                    Whitespace@4..9 "     ""#]],
        );
    }

    #[test]
    fn parse_binary_expression_with_whitespace() {
        check(
            " 1 +   2* 3 ",
            expect![[r#"
                Root@0..12
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
                      Asterisk@8..9 "*"
                      Whitespace@9..10 " "
                      Literal@10..12
                        IntegerLiteral@10..11 "3"
                        Whitespace@11..12 " ""#]],
        );
    }
}
