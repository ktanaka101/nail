use super::*;

pub(super) fn stmt(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(SyntaxKind::LetKw) {
        Some(variable_def(parser))
    } else {
        expr::expr(parser)
    }
}

fn variable_def(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::LetKw));
    let marker = parser.start();
    parser.bump();

    parser.expect(SyntaxKind::Ident);
    parser.expect(SyntaxKind::Equals);

    expr::expr(parser);

    marker.complete(parser, SyntaxKind::VariableDef)
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

    #[test]
    fn recover_on_let_token() {
        check(
            "let a =\nlet b = a",
            expect![[r#"
                Root@0..17
                  VariableDef@0..8
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Equals@6..7 "="
                    Whitespace@7..8 "\n"
                  VariableDef@8..17
                    LetKw@8..11 "let"
                    Whitespace@11..12 " "
                    Ident@12..13 "b"
                    Whitespace@13..14 " "
                    Equals@14..15 "="
                    Whitespace@15..16 " "
                    VariableRef@16..17
                      Ident@16..17 "a"
                error at 8..11: expected integerLiteral, identifier, '-' or '(', but found 'let'"#]],
        );
    }

    #[test]
    fn parse_multiple_statements() {
        check(
            "let a = 1\na",
            expect![[r#"
                Root@0..11
                  VariableDef@0..10
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Equals@6..7 "="
                    Whitespace@7..8 " "
                    Literal@8..10
                      IntegerLiteral@8..9 "1"
                      Whitespace@9..10 "\n"
                  VariableRef@10..11
                    Ident@10..11 "a""#]],
        );
    }
}
