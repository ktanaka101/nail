use super::Parser;
use crate::lexer2::SyntaxKind;

pub(super) fn expr(parser: &mut Parser) {
    expr_binding_power(parser, 0);
}

fn expr_binding_power(parser: &mut Parser, minimum_binding_power: u8) {
    let checkpoint = parser.checkpoint();

    match parser.peek() {
        Some(SyntaxKind::IntegerLiteral | SyntaxKind::Ident) => parser.bump(),
        Some(SyntaxKind::LParen | SyntaxKind::RParen) => {
            parser.bump();
            expr_binding_power(parser, 0);

            assert_eq!(parser.peek(), Some(SyntaxKind::RParen));
            parser.bump();
        }
        Some(SyntaxKind::Minus) => {
            let op = PrefixOp::Neg;
            let ((), right_binding_power) = op.binding_power();

            parser.bump();

            parser.start_node_at(checkpoint, SyntaxKind::PrefixExpr);
            expr_binding_power(parser, right_binding_power);
            parser.finish_node();
        }
        _ => (),
    }

    loop {
        let op = match parser.peek() {
            Some(SyntaxKind::Plus) => InfixOp::Add,
            Some(SyntaxKind::Minus) => InfixOp::Sub,
            Some(SyntaxKind::Asterisk) => InfixOp::Mul,
            Some(SyntaxKind::Slash) => InfixOp::Div,
            _ => return,
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            return;
        }

        parser.bump();

        parser.start_node_at(checkpoint, SyntaxKind::BinaryExpr);
        expr_binding_power(parser, right_binding_power);
        parser.finish_node();
    }
}

enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl InfixOp {
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

#[cfg(test)]
mod tests {
    use super::super::check;
    use expect_test::expect;

    #[test]
    fn parse_integer() {
        check(
            "123",
            expect![[r#"
                Root@0..3
                  IntegerLiteral@0..3 "123""#]],
        );
    }

    #[test]
    fn parse_variable_ref() {
        check(
            "counter",
            expect![[r#"
                Root@0..7
                  Ident@0..7 "counter""#]],
        )
    }

    #[test]
    fn parse_simple_binary_expression() {
        check(
            "1+2",
            expect![[r#"
                Root@0..3
                  BinaryExpr@0..3
                    IntegerLiteral@0..1 "1"
                    Plus@1..2 "+"
                    IntegerLiteral@2..3 "2""#]],
        );
    }

    #[test]
    fn parse_left_associative_binary_expression() {
        check(
            "1+2+3+4",
            expect![[r#"
                Root@0..7
                  BinaryExpr@0..7
                    BinaryExpr@0..5
                      BinaryExpr@0..3
                        IntegerLiteral@0..1 "1"
                        Plus@1..2 "+"
                        IntegerLiteral@2..3 "2"
                      Plus@3..4 "+"
                      IntegerLiteral@4..5 "3"
                    Plus@5..6 "+"
                    IntegerLiteral@6..7 "4""#]],
        );
    }

    #[test]
    fn parse_binary_expression_with_mixed_binding_power() {
        check(
            "1+2*3-4",
            expect![[r#"
                Root@0..7
                  BinaryExpr@0..7
                    BinaryExpr@0..5
                      IntegerLiteral@0..1 "1"
                      Plus@1..2 "+"
                      BinaryExpr@2..5
                        IntegerLiteral@2..3 "2"
                        Asterisk@3..4 "*"
                        IntegerLiteral@4..5 "3"
                    Minus@5..6 "-"
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
                    IntegerLiteral@1..3 "10""#]],
        );
    }

    #[test]
    fn negation_has_higher_binding_power_than_infix_operators() {
        check(
            "-20+20",
            expect![[r#"
                Root@0..6
                  BinaryExpr@0..6
                    PrefixExpr@0..3
                      Minus@0..1 "-"
                      IntegerLiteral@1..3 "20"
                    Plus@3..4 "+"
                    IntegerLiteral@4..6 "20""#]],
        );
    }

    #[test]
    fn parse_nested_parentheses() {
        check(
            "((((((10))))))",
            expect![[r#"
                Root@0..14
                  LParen@0..1 "("
                  LParen@1..2 "("
                  LParen@2..3 "("
                  LParen@3..4 "("
                  LParen@4..5 "("
                  LParen@5..6 "("
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
                  BinaryExpr@0..7
                    IntegerLiteral@0..1 "5"
                    Asterisk@1..2 "*"
                    LParen@2..3 "("
                    BinaryExpr@3..6
                      IntegerLiteral@3..4 "2"
                      Plus@4..5 "+"
                      IntegerLiteral@5..6 "1"
                    RParen@6..7 ")""#]],
        );
    }
}
