use super::*;

pub(super) fn stmt(parser: &mut Parser) -> Option<CompletedMarker> {
    match parser.peek() {
        Some(SyntaxKind::LetKw) => variable_def(parser),
        _ => expr::expr(parser),
    }
}

fn variable_def(parser: &mut Parser) -> Option<CompletedMarker> {
    assert!(parser.at(SyntaxKind::LetKw));
    let marker = parser.start();
    parser.bump();

    assert!(parser.at(SyntaxKind::Ident));
    parser.bump();

    assert!(parser.at(SyntaxKind::Equals));
    parser.bump();

    expr::expr(parser).unwrap();

    Some(marker.complete(parser, SyntaxKind::VariableDef))
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_variable_definition() {
        check(
            "let foo = bar",
            expect![[r#"
                Root@0..13
                  VariableDef@0..13
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..7 "foo"
                    Whitespace@7..8 " "
                    Equals@8..9 "="
                    Whitespace@9..10 " "
                    VariableRef@10..13
                      Ident@10..13 "bar""#]],
        );
    }
}
