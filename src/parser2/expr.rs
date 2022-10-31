use super::Parser;
use crate::lexer2::SyntaxKind;

pub(super) fn expr(parser: &mut Parser) {
    expr_binding_power(parser, 0);
}

fn expr_binding_power(parser: &mut Parser, minimum_binding_power: u8) {
    let checkpoint = parser.checkpoint();

    if let Some(SyntaxKind::IntegerLiteral | SyntaxKind::Ident) = parser.peek() {
        parser.bump();
    }

    loop {
        let op = match parser.peek() {
            Some(SyntaxKind::Plus) => Op::Add,
            Some(SyntaxKind::Minus) => Op::Sub,
            Some(SyntaxKind::Asterisk) => Op::Mul,
            Some(SyntaxKind::Slash) => Op::Div,
            _ => return,
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            return;
        }

        parser.bump();

        parser.start_node_at(checkpoint, SyntaxKind::BinOp);
        expr_binding_power(parser, right_binding_power);
        parser.finish_node();
    }
}

enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl Op {
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div => (3, 4),
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
    fn parse_binding_usage() {
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
                  BinOp@0..3
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
                  BinOp@0..7
                    BinOp@0..5
                      BinOp@0..3
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
                  BinOp@0..7
                    BinOp@0..5
                      IntegerLiteral@0..1 "1"
                      Plus@1..2 "+"
                      BinOp@2..5
                        IntegerLiteral@2..3 "2"
                        Asterisk@3..4 "*"
                        IntegerLiteral@4..5 "3"
                    Minus@5..6 "-"
                    IntegerLiteral@6..7 "4""#]],
        );
    }
}
