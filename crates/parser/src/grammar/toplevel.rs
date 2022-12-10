use lexer::TokenKind;

use super::stmt;
use crate::parser::{marker::CompletedMarker, Parser, TOPLEVEL_RECOVERY_SET};

pub(super) fn parse_stmt_on_toplevel(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(TokenKind::FnKw) {
        Some(stmt::parse_function_def(parser, &TOPLEVEL_RECOVERY_SET))
    } else {
        parser.error_with_recovery_set_only_default_on_toplevel();
        None
    }
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
                error at 0..3: expected 'fn', but found 'let'
                error at 4..7: expected 'fn', but found identifier
                error at 8..9: expected 'fn', but found '='
                error at 10..13: expected 'fn', but found identifier
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
}
