use super::Parser;
use crate::lexer2::SyntaxKind;

pub(crate) fn expr(parser: &mut Parser) {
    if let Some(SyntaxKind::IntegerLiteral | SyntaxKind::Ident) = parser.peek() {
        parser.bump();
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
}
