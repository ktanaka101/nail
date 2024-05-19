use lexer::TokenKind;
use syntax::SyntaxKind;

use crate::{
    grammar::{parse_path, stmt::parse_stmt_on_block},
    parser::{marker::CompletedNodeMarker, Parser, BLOCK_RECOVERY_SET},
};

/// 式の最初に現れる可能性があるトークンの集合
pub(super) const EXPR_FIRST: [TokenKind; 17] = [
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
    TokenKind::LoopKw,
    TokenKind::ContinueKw,
    TokenKind::BreakKw,
    TokenKind::WhileKw,
];

/// アイテムの最初に現れる可能性があるトークンの集合
pub(super) const ITEM_FIRST: &[TokenKind] = &[TokenKind::FnKw, TokenKind::ModKw];

/// 式のパース
pub(super) fn parse_expr(parser: &mut Parser) -> Option<CompletedNodeMarker> {
    parse_expr_binding_power(parser, 0, false)
}

/// 次の要素がブロックであっても、安全に式としてパースするための関数
///
/// ```ignore
/// let x = true;
/// if x {
///   10
/// }
/// ```
///
/// 以下のように、if式やwhile式では条件部分に構造体の初期化式を書くとボディブロックと構造体初期化ブロックが曖昧になるため、
/// 回避するために`parse_expr`ではなく`parse_expr_block_safe`を使用する。
/// ```ignore
/// struct A { x: bool }
/// if A { x: true }.x {
///   10
/// }
/// ```
///
/// 仕様としては、以下のように次の要素がブロックの場合は`()`で囲む必要がある。
/// ```ignore
/// struct A { x: bool }
/// if A { x: true }.x { // エラー
///   10
/// }
///
/// if (A { x: true }.x) { // OK
///   10
/// }
/// ```
///
fn parse_expr_block_safe(parser: &mut Parser) -> Option<CompletedNodeMarker> {
    parse_expr_binding_power(parser, 0, true)
}

/// 優先順位を考慮した式のパース
///
/// `minimum_binding_power`よりも結合力が弱い演算子が現れるまで式をパースします。
/// `minimum_binding_power`以上の結合力を持つ演算子が現れる限り結合し続けます。
///
/// `1 + 2 * 3 + 4`を例にすると、以下の各演算子の結合力は以下となります。
/// `1 +(5, 6) 2  *(7, 8) 3 +(5, 6) 4`
/// 以下のように結合されていきます。ノード自体は内側から構成されていきます。
/// `1 + 2`
/// `1 + 2 * 3`
/// `(1 + 2 * 3) + 4`
/// `(1 + (2 * 3)) + 4`
fn parse_expr_binding_power(
    parser: &mut Parser,
    minimum_binding_power: u8,
    block_safe: bool,
) -> Option<CompletedNodeMarker> {
    let mut lhs = parse_lhs(parser, block_safe)?;

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
        } else if parser.at(TokenKind::NotEq) {
            BinaryOp::NotEq
        } else if parser.at(TokenKind::RAngle) {
            BinaryOp::GreaterThan
        } else if parser.at(TokenKind::LAngle) {
            BinaryOp::LessThan
        } else if parser.at(TokenKind::GtEq) {
            BinaryOp::GtEq
        } else if parser.at(TokenKind::LtEq) {
            BinaryOp::LtEq
        } else if parser.at(TokenKind::Eq) {
            BinaryOp::Assign
        } else {
            break;
        };

        let (left_binding_power, right_binding_power) = op.binding_power();

        if left_binding_power < minimum_binding_power {
            break;
        }

        parser.bump();

        let marker = lhs.precede(parser);
        let parsed_rhs = parse_expr_binding_power(parser, right_binding_power, false);
        lhs = marker.complete(parser, SyntaxKind::BinaryExpr);

        if parsed_rhs.is_none() {
            break;
        }
    }

    Some(lhs)
}

/// 二項演算子
enum BinaryOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `==`
    Equal,
    /// `!=`
    NotEq,
    /// `>`
    GreaterThan,
    /// `<`
    LessThan,
    /// `>=`
    GtEq,
    /// `<=`
    LtEq,
    /// `=`
    Assign,
}

impl BinaryOp {
    /// 結合力を返します。
    /// 結合力はタプルの左側の要素が左結合力、右側の要素が右結合力です。
    /// 結合力が強いほど値が大きくなります。
    fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Assign => (2, 1),
            Self::Equal | Self::NotEq => (3, 4),
            Self::GreaterThan | Self::LessThan | Self::GtEq | Self::LtEq => (5, 6),
            Self::Add | Self::Sub => (7, 8),
            Self::Mul | Self::Div => (9, 10),
        }
    }
}

/// 単項演算子
enum PrefixOp {
    /// `-`
    Neg,
    /// `!`
    Not,
}

impl PrefixOp {
    /// 結合力を返します。
    /// 結合力はタプルの左側の要素が左結合力、右側の要素が右結合力です。
    /// 結合力が強いほど値が大きくなります。
    fn binding_power(&self) -> ((), u8) {
        match self {
            Self::Not => ((), 9),
            Self::Neg => ((), 10),
        }
    }
}

/// 左辺のパース
fn parse_lhs(parser: &mut Parser, block_safe: bool) -> Option<CompletedNodeMarker> {
    // 関数呼び出しの場合はCallノードとしたいが、そうでない場合はラップしたくないので、destroyを呼ぶ可能性がある
    // 以下のようなイメージ。
    // 関数呼び出しの場合: ExprStmt -> CallExpr   -> PathExpr -> ArgListExpr
    // 式の場合:         ExprStmt -> destroy対象 -> PathExpr
    let maybe_call_or_field_expr_marker = parser.start();

    let cm = if parser.at(TokenKind::IntegerLiteral)
        || parser.at(TokenKind::CharLiteral(false))
        || parser.at(TokenKind::StringLiteral)
        || parser.at(TokenKind::TrueKw)
        || parser.at(TokenKind::FalseKw)
    {
        parse_literal(parser)
    } else if !block_safe && parser.at(TokenKind::Ident) {
        parse_path_expr_or_record_expr(parser)
    } else if block_safe && parser.at(TokenKind::Ident) {
        parse_path_expr(parser)
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
    } else if parser.at(TokenKind::LoopKw) {
        parse_loop(parser)
    } else if parser.at(TokenKind::ContinueKw) {
        parse_continue(parser)
    } else if parser.at(TokenKind::BreakKw) {
        parse_break(parser)
    } else if parser.at(TokenKind::WhileKw) {
        parse_while(parser)
    } else {
        parser.error_with_recovery_set_only_default_on_block();
        maybe_call_or_field_expr_marker.destroy(parser);
        return None;
    };

    match parser.peek() {
        Some(TokenKind::LParen) => {
            parse_args(parser);
            Some(maybe_call_or_field_expr_marker.complete(parser, SyntaxKind::CallExpr))
        }
        Some(TokenKind::Dot) => {
            parser.bump();
            parser.expect_on_block(TokenKind::Ident);
            Some(maybe_call_or_field_expr_marker.complete(parser, SyntaxKind::FieldExpr))
        }
        _ => {
            maybe_call_or_field_expr_marker.destroy(parser);
            Some(cm)
        }
    }
}

/// リテラルのパース
fn parse_literal(parser: &mut Parser) -> CompletedNodeMarker {
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

/// リテラルのバリデーション
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

/// パス式あるいはレコード式のパース
///
/// 以下は両方のパースが可能です。区別をするためには、`{`が続くかどうかで判断します。
/// - RecordExpr: `let point = Point { x: 10, y: 20 };`
/// - PathExpr: `let point = Point`
fn parse_path_expr_or_record_expr(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::Ident));

    let marker = parser.start();

    parse_path(parser, &BLOCK_RECOVERY_SET);

    if parser.at(TokenKind::LCurly) {
        // Pathの次に`{`がある場合はRecordExprとして返す
        parse_record_field_list_expr(parser);
        marker.complete(parser, SyntaxKind::RecordExpr)
    } else {
        // Pathの次にパス式の場合はPathExprとして返す
        marker.complete(parser, SyntaxKind::PathExpr)
    }
}

/// パス式のパース
fn parse_path_expr(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::Ident));

    let marker = parser.start();
    parse_path(parser, &BLOCK_RECOVERY_SET);
    marker.complete(parser, SyntaxKind::PathExpr)
}

/// 関数呼び出しの引数のパース
/// `(arg1, arg2, ...)`
fn parse_args(parser: &mut Parser) -> CompletedNodeMarker {
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

/// 単項演算式のパース
fn parse_prefix_expr(parser: &mut Parser) -> CompletedNodeMarker {
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

    parse_expr_binding_power(parser, right_binding_power, false);

    marker.complete(parser, SyntaxKind::UnaryExpr)
}

/// `()`で囲まれている式のパース
///
/// `()`の中の式は`()`と結合させるため、最小の結合力である`0`で呼び出しています。
fn parse_paren_expr(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::LParen));

    let marker = parser.start();
    parser.bump();
    parse_expr_binding_power(parser, 0, false);
    parser.expect_on_block(TokenKind::RParen);

    marker.complete(parser, SyntaxKind::ParenExpr)
}

/// ブロックのパース
///
/// ブロックはさまざまな箇所で使用されています。
/// `if-else`式や、`while`式、関数などです。
fn parse_block(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::LCurly));

    let marker = parser.start();
    parser.bump();
    while !parser.at(TokenKind::RCurly) && !parser.at_end() {
        parse_stmt_on_block(parser);
    }
    parser.expect_on_block(TokenKind::RCurly);

    marker.complete(parser, SyntaxKind::BlockExpr)
}

/// `if`式のパース
fn parse_if(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::IfKw));

    let marker = parser.start();
    parser.bump();
    parse_expr_block_safe(parser);

    if parser.at(TokenKind::LCurly) {
        parse_block(parser);
    }

    if parser.at(TokenKind::ElseKw) {
        parser.bump();
        // elseのブロックがなくても続きを食わない事で、続きの一連のトークンに影響を及ぼさない
        if parser.at(TokenKind::LCurly) {
            parse_block(parser);
        }
    }

    marker.complete(parser, SyntaxKind::IfExpr)
}

/// `return`式のパース
fn parse_return(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::ReturnKw));

    let marker = parser.start();
    parser.bump();
    if parser.at_set_no_expected(&EXPR_FIRST) {
        parse_expr(parser);
    }

    marker.complete(parser, SyntaxKind::ReturnExpr)
}

/// `loop`式のパース
fn parse_loop(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::LoopKw));

    let marker = parser.start();
    parser.bump();

    if parser.at(TokenKind::LCurly) {
        parse_block(parser);
    }

    marker.complete(parser, SyntaxKind::LoopExpr)
}

/// `continue`式のパース
fn parse_continue(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::ContinueKw));

    let marker = parser.start();
    parser.bump();

    marker.complete(parser, SyntaxKind::ContinueExpr)
}

/// `break`式のパース
fn parse_break(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::BreakKw));

    let marker = parser.start();
    parser.bump();

    if parser.at_set_no_expected(&EXPR_FIRST) {
        parse_expr(parser);
    }

    marker.complete(parser, SyntaxKind::BreakExpr)
}

/// `while`式のパース
fn parse_while(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::WhileKw));

    let marker = parser.start();
    parser.bump();
    parse_expr_block_safe(parser);

    if parser.at(TokenKind::LCurly) {
        parse_block(parser);
    }

    marker.complete(parser, SyntaxKind::WhileExpr)
}

fn parse_record_field_list_expr(parser: &mut Parser) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::LCurly));

    let marker = parser.start();

    parser.expect_on_block(TokenKind::LCurly);

    if parser.at(TokenKind::Ident)
        || parser.peek() == Some(TokenKind::Colon)
        || parser.peek() == Some(TokenKind::Comma)
    {
        {
            let marker = parser.start();
            parser.expect_on_block(TokenKind::Ident);
            parser.expect_on_block(TokenKind::Colon);
            parse_expr(parser);
            marker.complete(parser, SyntaxKind::RecordFieldExpr);
        }

        while parser.at(TokenKind::Comma) {
            parser.bump();
            // Point { x: 10, y: 20, }
            //                     ^,時点で停止させる
            if parser.at(TokenKind::RCurly) {
                break;
            }

            {
                let marker = parser.start();
                parser.expect_on_block(TokenKind::Ident);
                parser.expect_on_block(TokenKind::Colon);
                parse_expr(parser);
                marker.complete(parser, SyntaxKind::RecordFieldExpr);
            }
        }
    }

    parser.expect_on_block(TokenKind::RCurly);

    marker.complete(parser, SyntaxKind::RecordFieldListExpr)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::check_debug_tree_in_block;

    #[test]
    fn parse_integer() {
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
            "1 + 2 - 3 * 4 / 5 == 6 != 7 > 8 < 9 >= 10 <= 11",
            expect![[r#"
                SourceFile@0..47
                  ExprStmt@0..47
                    BinaryExpr@0..47
                      BinaryExpr@0..22
                        BinaryExpr@0..17
                          BinaryExpr@0..5
                            Literal@0..1
                              Integer@0..1 "1"
                            Whitespace@1..2 " "
                            Plus@2..3 "+"
                            Whitespace@3..4 " "
                            Literal@4..5
                              Integer@4..5 "2"
                          Whitespace@5..6 " "
                          Minus@6..7 "-"
                          Whitespace@7..8 " "
                          BinaryExpr@8..17
                            BinaryExpr@8..13
                              Literal@8..9
                                Integer@8..9 "3"
                              Whitespace@9..10 " "
                              Star@10..11 "*"
                              Whitespace@11..12 " "
                              Literal@12..13
                                Integer@12..13 "4"
                            Whitespace@13..14 " "
                            Slash@14..15 "/"
                            Whitespace@15..16 " "
                            Literal@16..17
                              Integer@16..17 "5"
                        Whitespace@17..18 " "
                        Eq2@18..20 "=="
                        Whitespace@20..21 " "
                        Literal@21..22
                          Integer@21..22 "6"
                      Whitespace@22..23 " "
                      NotEq@23..25 "!="
                      Whitespace@25..26 " "
                      BinaryExpr@26..47
                        BinaryExpr@26..41
                          BinaryExpr@26..35
                            BinaryExpr@26..31
                              Literal@26..27
                                Integer@26..27 "7"
                              Whitespace@27..28 " "
                              RAngle@28..29 ">"
                              Whitespace@29..30 " "
                              Literal@30..31
                                Integer@30..31 "8"
                            Whitespace@31..32 " "
                            LAngle@32..33 "<"
                            Whitespace@33..34 " "
                            Literal@34..35
                              Integer@34..35 "9"
                          Whitespace@35..36 " "
                          GtEq@36..38 ">="
                          Whitespace@38..39 " "
                          Literal@39..41
                            Integer@39..41 "10"
                        Whitespace@41..42 " "
                        LtEq@42..44 "<="
                        Whitespace@44..45 " "
                        Literal@45..47
                          Integer@45..47 "11"
            "#]],
        );
    }

    #[test]
    fn parse_left_associative_binary_expression() {
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
            "1 + 2 == 3 * 4",
            expect![[r#"
                SourceFile@0..14
                  ExprStmt@0..14
                    BinaryExpr@0..14
                      BinaryExpr@0..5
                        Literal@0..1
                          Integer@0..1 "1"
                        Whitespace@1..2 " "
                        Plus@2..3 "+"
                        Whitespace@3..4 " "
                        Literal@4..5
                          Integer@4..5 "2"
                      Whitespace@5..6 " "
                      Eq2@6..8 "=="
                      Whitespace@8..9 " "
                      BinaryExpr@9..14
                        Literal@9..10
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
            "999   ",
            expect![[r#"
                SourceFile@0..6
                  ExprStmt@0..3
                    Literal@0..3
                      Integer@0..3 "999"
                  Whitespace@3..6 "   "
            "#]],
        );
    }

    #[test]
    fn parse_integer_surrounded_by_whitespace() {
        check_debug_tree_in_block(
            " 123     ",
            expect![[r#"
                SourceFile@0..9
                  Whitespace@0..1 " "
                  ExprStmt@1..4
                    Literal@1..4
                      Integer@1..4 "123"
                  Whitespace@4..9 "     "
            "#]],
        );
    }

    #[test]
    fn parse_char() {
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
            "'a let b = 10",
            expect![[r#"
                SourceFile@0..13
                  ExprStmt@0..2
                    Literal@0..2
                      Char@0..2 "'a"
                  Whitespace@2..3 " "
                  Let@3..13
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
        check_debug_tree_in_block(
            " 1 +   2* 3 ",
            expect![[r#"
                SourceFile@0..12
                  Whitespace@0..1 " "
                  ExprStmt@1..11
                    BinaryExpr@1..11
                      Literal@1..2
                        Integer@1..2 "1"
                      Whitespace@2..3 " "
                      Plus@3..4 "+"
                      Whitespace@4..7 "   "
                      BinaryExpr@7..11
                        Literal@7..8
                          Integer@7..8 "2"
                        Star@8..9 "*"
                        Whitespace@9..10 " "
                        Literal@10..11
                          Integer@10..11 "3"
                  Whitespace@11..12 " "
            "#]],
        );
    }

    #[test]
    fn parse_binary_expression_assign() {
        check_debug_tree_in_block(
            "a = 10",
            expect![[r#"
                SourceFile@0..6
                  ExprStmt@0..6
                    BinaryExpr@0..6
                      PathExpr@0..1
                        Path@0..1
                          PathSegment@0..1
                            Ident@0..1 "a"
                      Whitespace@1..2 " "
                      Eq@2..3 "="
                      Whitespace@3..4 " "
                      Literal@4..6
                        Integer@4..6 "10"
            "#]],
        );
    }

    #[test]
    fn parse_binary_expression_assign_right_to_left() {
        check_debug_tree_in_block(
            "a = b = 20",
            expect![[r#"
                SourceFile@0..10
                  ExprStmt@0..10
                    BinaryExpr@0..10
                      PathExpr@0..1
                        Path@0..1
                          PathSegment@0..1
                            Ident@0..1 "a"
                      Whitespace@1..2 " "
                      Eq@2..3 "="
                      Whitespace@3..4 " "
                      BinaryExpr@4..10
                        PathExpr@4..5
                          Path@4..5
                            PathSegment@4..5
                              Ident@4..5 "b"
                        Whitespace@5..6 " "
                        Eq@6..7 "="
                        Whitespace@7..8 " "
                        Literal@8..10
                          Integer@8..10 "20"
            "#]],
        );
    }

    #[test]
    fn parse_call_in_binary() {
        check_debug_tree_in_block(
            "foo(1) + bar(2)",
            expect![[r#"
                SourceFile@0..15
                  ExprStmt@0..15
                    BinaryExpr@0..15
                      CallExpr@0..6
                        PathExpr@0..3
                          Path@0..3
                            PathSegment@0..3
                              Ident@0..3 "foo"
                        ArgList@3..6
                          LParen@3..4 "("
                          Arg@4..5
                            Literal@4..5
                              Integer@4..5 "1"
                          RParen@5..6 ")"
                      Whitespace@6..7 " "
                      Plus@7..8 "+"
                      Whitespace@8..9 " "
                      CallExpr@9..15
                        PathExpr@9..12
                          Path@9..12
                            PathSegment@9..12
                              Ident@9..12 "bar"
                        ArgList@12..15
                          LParen@12..13 "("
                          Arg@13..14
                            Literal@13..14
                              Integer@13..14 "2"
                          RParen@14..15 ")"
            "#]],
        );
    }

    #[test]
    fn parse_unclosed_parentheses() {
        check_debug_tree_in_block(
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
                error at 1..4: expected '::', '{', '+', '-', '*', '/', '==', '!=', '>', '<', '>=', '<=', '=' or ')'
            "#]],
        );
    }

    #[test]
    fn parse_multi_recover() {
        check_debug_tree_in_block(
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
                error at 2..3: expected integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '!', '(', '{', 'if', 'return', 'loop', 'continue', 'break' or 'while'
                error at 2..3: expected ')'
            "#]],
        );
    }

    #[test]
    fn parse_block() {
        check_debug_tree_in_block(
            "{ 1 }",
            expect![[r#"
                SourceFile@0..5
                  ExprStmt@0..5
                    BlockExpr@0..5
                      LCurly@0..1 "{"
                      Whitespace@1..2 " "
                      ExprStmt@2..3
                        Literal@2..3
                          Integer@2..3 "1"
                      Whitespace@3..4 " "
                      RCurly@4..5 "}"
            "#]],
        );
    }

    #[test]
    fn parse_block_missing_close() {
        check_debug_tree_in_block(
            "{ 1 ",
            expect![[r#"
                SourceFile@0..4
                  ExprStmt@0..3
                    BlockExpr@0..3
                      LCurly@0..1 "{"
                      Whitespace@1..2 " "
                      ExprStmt@2..3
                        Literal@2..3
                          Integer@2..3 "1"
                  Whitespace@3..4 " "
                error at 3..4: expected '+', '-', '*', '/', '==', '!=', '>', '<', '>=', '<=', '=', ';' or '}'
            "#]],
        );
    }

    #[test]
    fn parse_nested_block() {
        check_debug_tree_in_block(
            r#"{
  let a = 10
  { 1 }
}"#,
            expect![[r#"
                SourceFile@0..24
                  ExprStmt@0..24
                    BlockExpr@0..24
                      LCurly@0..1 "{"
                      Whitespace@1..4 "\n  "
                      Let@4..14
                        LetKw@4..7 "let"
                        Whitespace@7..8 " "
                        Ident@8..9 "a"
                        Whitespace@9..10 " "
                        Eq@10..11 "="
                        Whitespace@11..12 " "
                        Literal@12..14
                          Integer@12..14 "10"
                      Whitespace@14..17 "\n  "
                      ExprStmt@17..22
                        BlockExpr@17..22
                          LCurly@17..18 "{"
                          Whitespace@18..19 " "
                          ExprStmt@19..20
                            Literal@19..20
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
        check_debug_tree_in_block(
            "a()",
            expect![[r#"
                SourceFile@0..3
                  ExprStmt@0..3
                    CallExpr@0..3
                      PathExpr@0..1
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
        check_debug_tree_in_block(
            "a(x, y)",
            expect![[r#"
                SourceFile@0..7
                  ExprStmt@0..7
                    CallExpr@0..7
                      PathExpr@0..1
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

        check_debug_tree_in_block(
            r#"aaa("aaa", true, 'a', 10)"#,
            expect![[r#"
                SourceFile@0..25
                  ExprStmt@0..25
                    CallExpr@0..25
                      PathExpr@0..3
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
    fn parse_call_by_block() {
        check_debug_tree_in_block(
            "{ a }()",
            expect![[r#"
                SourceFile@0..7
                  ExprStmt@0..7
                    CallExpr@0..7
                      BlockExpr@0..5
                        LCurly@0..1 "{"
                        Whitespace@1..2 " "
                        ExprStmt@2..3
                          PathExpr@2..3
                            Path@2..3
                              PathSegment@2..3
                                Ident@2..3 "a"
                        Whitespace@3..4 " "
                        RCurly@4..5 "}"
                      ArgList@5..7
                        LParen@5..6 "("
                        RParen@6..7 ")"
            "#]],
        );
    }

    #[test]
    fn parse_call_missing_rparen() {
        check_debug_tree_in_block(
            "a(x, y",
            expect![[r#"
                SourceFile@0..6
                  ExprStmt@0..6
                    CallExpr@0..6
                      PathExpr@0..1
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
                error at 5..6: expected '::', '{', '+', '-', '*', '/', '==', '!=', '>', '<', '>=', '<=', '=', ',' or ')'
            "#]],
        );

        check_debug_tree_in_block(
            "a(x,",
            expect![[r#"
                SourceFile@0..4
                  ExprStmt@0..4
                    CallExpr@0..4
                      PathExpr@0..1
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

        check_debug_tree_in_block(
            "a(x",
            expect![[r#"
                SourceFile@0..3
                  ExprStmt@0..3
                    CallExpr@0..3
                      PathExpr@0..1
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
                error at 2..3: expected '::', '{', '+', '-', '*', '/', '==', '!=', '>', '<', '>=', '<=', '=', ',' or ')'
            "#]],
        );

        check_debug_tree_in_block(
            "a(",
            expect![[r#"
                SourceFile@0..2
                  ExprStmt@0..2
                    CallExpr@0..2
                      PathExpr@0..1
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
        check_debug_tree_in_block(
            "a(x,)",
            expect![[r#"
                SourceFile@0..5
                  ExprStmt@0..5
                    CallExpr@0..5
                      PathExpr@0..1
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
        check_debug_tree_in_block(
            "a(x y)",
            expect![[r#"
                SourceFile@0..6
                  ExprStmt@0..5
                    CallExpr@0..5
                      PathExpr@0..1
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
                        Whitespace@3..4 " "
                        Error@4..5
                          Ident@4..5 "y"
                  ExprStmt@5..6
                    Error@5..6
                      RParen@5..6 ")"
                error at 4..5: expected '::', '{', '+', '-', '*', '/', '==', '!=', '>', '<', '>=', '<=', '=', ',' or ')', but found identifier
                error at 5..6: expected '+', '-', '*', '/', '==', '!=', '>', '<', '>=', '<=', '=', ';', 'let', 'fn', 'struct', 'mod', integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '!', '(', '{', 'if', 'return', 'loop', 'continue', 'break' or 'while', but found ')'
            "#]],
        );
    }

    #[test]
    fn parse_expr_on_arg() {
        check_debug_tree_in_block(
            "a(x + y)",
            expect![[r#"
                SourceFile@0..8
                  ExprStmt@0..8
                    CallExpr@0..8
                      PathExpr@0..1
                        Path@0..1
                          PathSegment@0..1
                            Ident@0..1 "a"
                      ArgList@1..8
                        LParen@1..2 "("
                        Arg@2..7
                          BinaryExpr@2..7
                            PathExpr@2..3
                              Path@2..3
                                PathSegment@2..3
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

        check_debug_tree_in_block(
            "a({ x + y })",
            expect![[r#"
                SourceFile@0..12
                  ExprStmt@0..12
                    CallExpr@0..12
                      PathExpr@0..1
                        Path@0..1
                          PathSegment@0..1
                            Ident@0..1 "a"
                      ArgList@1..12
                        LParen@1..2 "("
                        Arg@2..11
                          BlockExpr@2..11
                            LCurly@2..3 "{"
                            Whitespace@3..4 " "
                            ExprStmt@4..9
                              BinaryExpr@4..9
                                PathExpr@4..5
                                  Path@4..5
                                    PathSegment@4..5
                                      Ident@4..5 "x"
                                Whitespace@5..6 " "
                                Plus@6..7 "+"
                                Whitespace@7..8 " "
                                PathExpr@8..9
                                  Path@8..9
                                    PathSegment@8..9
                                      Ident@8..9 "y"
                            Whitespace@9..10 " "
                            RCurly@10..11 "}"
                        RParen@11..12 ")"
            "#]],
        );
    }

    #[test]
    fn parse_if_expr() {
        check_debug_tree_in_block(
            "if true { 10 }",
            expect![[r#"
                SourceFile@0..14
                  ExprStmt@0..14
                    IfExpr@0..14
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      Literal@3..7
                        TrueKw@3..7 "true"
                      Whitespace@7..8 " "
                      BlockExpr@8..14
                        LCurly@8..9 "{"
                        Whitespace@9..10 " "
                        ExprStmt@10..12
                          Literal@10..12
                            Integer@10..12 "10"
                        Whitespace@12..13 " "
                        RCurly@13..14 "}"
            "#]],
        );

        check_debug_tree_in_block(
            "if true { 10 } else { 20 }",
            expect![[r#"
                SourceFile@0..26
                  ExprStmt@0..26
                    IfExpr@0..26
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      Literal@3..7
                        TrueKw@3..7 "true"
                      Whitespace@7..8 " "
                      BlockExpr@8..14
                        LCurly@8..9 "{"
                        Whitespace@9..10 " "
                        ExprStmt@10..12
                          Literal@10..12
                            Integer@10..12 "10"
                        Whitespace@12..13 " "
                        RCurly@13..14 "}"
                      Whitespace@14..15 " "
                      ElseKw@15..19 "else"
                      Whitespace@19..20 " "
                      BlockExpr@20..26
                        LCurly@20..21 "{"
                        Whitespace@21..22 " "
                        ExprStmt@22..24
                          Literal@22..24
                            Integer@22..24 "20"
                        Whitespace@24..25 " "
                        RCurly@25..26 "}"
            "#]],
        )
    }

    #[test]
    fn parse_if_expr_condition_variable() {
        check_debug_tree_in_block(
            "let a = true; if a { 10 }",
            expect![[r#"
                SourceFile@0..25
                  Let@0..13
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Eq@6..7 "="
                    Whitespace@7..8 " "
                    Literal@8..12
                      TrueKw@8..12 "true"
                    Semicolon@12..13 ";"
                  Whitespace@13..14 " "
                  ExprStmt@14..25
                    IfExpr@14..25
                      IfKw@14..16 "if"
                      Whitespace@16..17 " "
                      PathExpr@17..18
                        Path@17..18
                          PathSegment@17..18
                            Ident@17..18 "a"
                      Whitespace@18..19 " "
                      BlockExpr@19..25
                        LCurly@19..20 "{"
                        Whitespace@20..21 " "
                        ExprStmt@21..23
                          Literal@21..23
                            Integer@21..23 "10"
                        Whitespace@23..24 " "
                        RCurly@24..25 "}"
            "#]],
        );
    }

    #[test]
    fn parse_if_expr_condition_struct_record_init_is_error() {
        check_debug_tree_in_block(
            r#"
                struct A { x: bool }
                if A { x: true } { 10 }
            "#,
            expect![[r#"
                SourceFile@0..90
                  Whitespace@0..17 "\n                "
                  StructDef@17..37
                    StructKw@17..23 "struct"
                    Whitespace@23..24 " "
                    Ident@24..25 "A"
                    Whitespace@25..26 " "
                    RecordFieldList@26..37
                      LCurly@26..27 "{"
                      Whitespace@27..28 " "
                      RecordField@28..35
                        Ident@28..29 "x"
                        Colon@29..30 ":"
                        Whitespace@30..31 " "
                        PathType@31..35
                          Path@31..35
                            PathSegment@31..35
                              Ident@31..35 "bool"
                      Whitespace@35..36 " "
                      RCurly@36..37 "}"
                  Whitespace@37..54 "\n                "
                  ExprStmt@54..70
                    IfExpr@54..70
                      IfKw@54..56 "if"
                      Whitespace@56..57 " "
                      PathExpr@57..58
                        Path@57..58
                          PathSegment@57..58
                            Ident@57..58 "A"
                      Whitespace@58..59 " "
                      BlockExpr@59..70
                        LCurly@59..60 "{"
                        Whitespace@60..61 " "
                        ExprStmt@61..62
                          PathExpr@61..62
                            Path@61..62
                              PathSegment@61..62
                                Ident@61..62 "x"
                        ExprStmt@62..63
                          Error@62..63
                            Colon@62..63 ":"
                        Whitespace@63..64 " "
                        ExprStmt@64..68
                          Literal@64..68
                            TrueKw@64..68 "true"
                        Whitespace@68..69 " "
                        RCurly@69..70 "}"
                  Whitespace@70..71 " "
                  ExprStmt@71..77
                    BlockExpr@71..77
                      LCurly@71..72 "{"
                      Whitespace@72..73 " "
                      ExprStmt@73..75
                        Literal@73..75
                          Integer@73..75 "10"
                      Whitespace@75..76 " "
                      RCurly@76..77 "}"
                  Whitespace@77..90 "\n            "
                error at 62..63: expected '::', '{', '+', '-', '*', '/', '==', '!=', '>', '<', '>=', '<=', '=', ';', '}', 'let', 'fn', 'struct', 'mod', integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '!', '(', 'if', 'return', 'loop', 'continue', 'break' or 'while', but found ':'
            "#]],
        );
    }

    #[test]
    fn parse_if_expr_condition_struct_record_init_around_paren_is_ok() {
        check_debug_tree_in_block(
            r#"
                struct A { x: bool }
                if (A { x: true }) { 10 }
            "#,
            expect![[r#"
                SourceFile@0..92
                  Whitespace@0..17 "\n                "
                  StructDef@17..37
                    StructKw@17..23 "struct"
                    Whitespace@23..24 " "
                    Ident@24..25 "A"
                    Whitespace@25..26 " "
                    RecordFieldList@26..37
                      LCurly@26..27 "{"
                      Whitespace@27..28 " "
                      RecordField@28..35
                        Ident@28..29 "x"
                        Colon@29..30 ":"
                        Whitespace@30..31 " "
                        PathType@31..35
                          Path@31..35
                            PathSegment@31..35
                              Ident@31..35 "bool"
                      Whitespace@35..36 " "
                      RCurly@36..37 "}"
                  Whitespace@37..54 "\n                "
                  ExprStmt@54..79
                    IfExpr@54..79
                      IfKw@54..56 "if"
                      Whitespace@56..57 " "
                      ParenExpr@57..72
                        LParen@57..58 "("
                        RecordExpr@58..71
                          Path@58..59
                            PathSegment@58..59
                              Ident@58..59 "A"
                          Whitespace@59..60 " "
                          RecordFieldListExpr@60..71
                            LCurly@60..61 "{"
                            Whitespace@61..62 " "
                            RecordFieldExpr@62..69
                              Ident@62..63 "x"
                              Colon@63..64 ":"
                              Whitespace@64..65 " "
                              Literal@65..69
                                TrueKw@65..69 "true"
                            Whitespace@69..70 " "
                            RCurly@70..71 "}"
                        RParen@71..72 ")"
                      Whitespace@72..73 " "
                      BlockExpr@73..79
                        LCurly@73..74 "{"
                        Whitespace@74..75 " "
                        ExprStmt@75..77
                          Literal@75..77
                            Integer@75..77 "10"
                        Whitespace@77..78 " "
                        RCurly@78..79 "}"
                  Whitespace@79..92 "\n            "
            "#]],
        );
    }

    #[test]
    fn parse_if_is_expr() {
        check_debug_tree_in_block(
            "let a = if true { 10 } else { 20 }",
            expect![[r#"
                SourceFile@0..34
                  Let@0..34
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Eq@6..7 "="
                    Whitespace@7..8 " "
                    IfExpr@8..34
                      IfKw@8..10 "if"
                      Whitespace@10..11 " "
                      Literal@11..15
                        TrueKw@11..15 "true"
                      Whitespace@15..16 " "
                      BlockExpr@16..22
                        LCurly@16..17 "{"
                        Whitespace@17..18 " "
                        ExprStmt@18..20
                          Literal@18..20
                            Integer@18..20 "10"
                        Whitespace@20..21 " "
                        RCurly@21..22 "}"
                      Whitespace@22..23 " "
                      ElseKw@23..27 "else"
                      Whitespace@27..28 " "
                      BlockExpr@28..34
                        LCurly@28..29 "{"
                        Whitespace@29..30 " "
                        ExprStmt@30..32
                          Literal@30..32
                            Integer@30..32 "20"
                        Whitespace@32..33 " "
                        RCurly@33..34 "}"
            "#]],
        )
    }

    #[test]
    fn parse_if_expr_missing_condition() {
        check_debug_tree_in_block(
            "if { 10 } else { 20 }",
            expect![[r#"
                SourceFile@0..21
                  ExprStmt@0..21
                    IfExpr@0..21
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      BlockExpr@3..9
                        LCurly@3..4 "{"
                        Whitespace@4..5 " "
                        ExprStmt@5..7
                          Literal@5..7
                            Integer@5..7 "10"
                        Whitespace@7..8 " "
                        RCurly@8..9 "}"
                      Whitespace@9..10 " "
                      ElseKw@10..14 "else"
                      Whitespace@14..15 " "
                      BlockExpr@15..21
                        LCurly@15..16 "{"
                        Whitespace@16..17 " "
                        ExprStmt@17..19
                          Literal@17..19
                            Integer@17..19 "20"
                        Whitespace@19..20 " "
                        RCurly@20..21 "}"
            "#]],
        );
    }

    #[test]
    fn parse_if_expr_missing_then_block() {
        check_debug_tree_in_block(
            "if true else { 20 }",
            expect![[r#"
                SourceFile@0..19
                  ExprStmt@0..19
                    IfExpr@0..19
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      Literal@3..7
                        TrueKw@3..7 "true"
                      Whitespace@7..8 " "
                      ElseKw@8..12 "else"
                      Whitespace@12..13 " "
                      BlockExpr@13..19
                        LCurly@13..14 "{"
                        Whitespace@14..15 " "
                        ExprStmt@15..17
                          Literal@15..17
                            Integer@15..17 "20"
                        Whitespace@17..18 " "
                        RCurly@18..19 "}"
            "#]],
        );
    }

    #[test]
    fn parse_if_expr_missing_else() {
        check_debug_tree_in_block(
            "if true { 10 } { 20 }",
            expect![[r#"
                SourceFile@0..21
                  ExprStmt@0..14
                    IfExpr@0..14
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      Literal@3..7
                        TrueKw@3..7 "true"
                      Whitespace@7..8 " "
                      BlockExpr@8..14
                        LCurly@8..9 "{"
                        Whitespace@9..10 " "
                        ExprStmt@10..12
                          Literal@10..12
                            Integer@10..12 "10"
                        Whitespace@12..13 " "
                        RCurly@13..14 "}"
                  Whitespace@14..15 " "
                  ExprStmt@15..21
                    BlockExpr@15..21
                      LCurly@15..16 "{"
                      Whitespace@16..17 " "
                      ExprStmt@17..19
                        Literal@17..19
                          Integer@17..19 "20"
                      Whitespace@19..20 " "
                      RCurly@20..21 "}"
            "#]],
        );
    }

    #[test]
    fn parse_if_expr_missing_else_block() {
        check_debug_tree_in_block(
            "if true { 10 } else",
            expect![[r#"
                SourceFile@0..19
                  ExprStmt@0..19
                    IfExpr@0..19
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      Literal@3..7
                        TrueKw@3..7 "true"
                      Whitespace@7..8 " "
                      BlockExpr@8..14
                        LCurly@8..9 "{"
                        Whitespace@9..10 " "
                        ExprStmt@10..12
                          Literal@10..12
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
        check_debug_tree_in_block(
            "if { true } { 10 } else { 20 }",
            expect![[r#"
                SourceFile@0..30
                  ExprStmt@0..30
                    IfExpr@0..30
                      IfKw@0..2 "if"
                      Whitespace@2..3 " "
                      BlockExpr@3..11
                        LCurly@3..4 "{"
                        Whitespace@4..5 " "
                        ExprStmt@5..9
                          Literal@5..9
                            TrueKw@5..9 "true"
                        Whitespace@9..10 " "
                        RCurly@10..11 "}"
                      Whitespace@11..12 " "
                      BlockExpr@12..18
                        LCurly@12..13 "{"
                        Whitespace@13..14 " "
                        ExprStmt@14..16
                          Literal@14..16
                            Integer@14..16 "10"
                        Whitespace@16..17 " "
                        RCurly@17..18 "}"
                      Whitespace@18..19 " "
                      ElseKw@19..23 "else"
                      Whitespace@23..24 " "
                      BlockExpr@24..30
                        LCurly@24..25 "{"
                        Whitespace@25..26 " "
                        ExprStmt@26..28
                          Literal@26..28
                            Integer@26..28 "20"
                        Whitespace@28..29 " "
                        RCurly@29..30 "}"
            "#]],
        );
    }

    #[test]
    fn parse_equal() {
        check_debug_tree_in_block(
            "10 == 20",
            expect![[r#"
                SourceFile@0..8
                  ExprStmt@0..8
                    BinaryExpr@0..8
                      Literal@0..2
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
    fn parse_not_equal() {
        check_debug_tree_in_block(
            "10 != 20",
            expect![[r#"
                SourceFile@0..8
                  ExprStmt@0..8
                    BinaryExpr@0..8
                      Literal@0..2
                        Integer@0..2 "10"
                      Whitespace@2..3 " "
                      NotEq@3..5 "!="
                      Whitespace@5..6 " "
                      Literal@6..8
                        Integer@6..8 "20"
            "#]],
        );
    }

    #[test]
    fn parse_return() {
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
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
                  ExprStmt@17..26
                    ReturnExpr@17..26
                      ReturnKw@17..23 "return"
                      Whitespace@23..24 " "
                      Literal@24..26
                        Integer@24..26 "10"
                  Whitespace@26..43 "\n                "
                  ExprStmt@43..53
                    ReturnExpr@43..53
                      ReturnKw@43..49 "return"
                      Whitespace@49..50 " "
                      Literal@50..53
                        Char@50..53 "'a'"
                  Whitespace@53..70 "\n                "
                  ExprStmt@70..82
                    ReturnExpr@70..82
                      ReturnKw@70..76 "return"
                      Whitespace@76..77 " "
                      Literal@77..82
                        String@77..82 "\"aaa\""
                  Whitespace@82..99 "\n                "
                  ExprStmt@99..110
                    ReturnExpr@99..110
                      ReturnKw@99..105 "return"
                      Whitespace@105..106 " "
                      Literal@106..110
                        TrueKw@106..110 "true"
                  Whitespace@110..127 "\n                "
                  ExprStmt@127..139
                    ReturnExpr@127..139
                      ReturnKw@127..133 "return"
                      Whitespace@133..134 " "
                      Literal@134..139
                        FalseKw@134..139 "false"
                  Whitespace@139..156 "\n                "
                  ExprStmt@156..166
                    ReturnExpr@156..166
                      ReturnKw@156..162 "return"
                      Whitespace@162..163 " "
                      UnaryExpr@163..166
                        Minus@163..164 "-"
                        Literal@164..166
                          Integer@164..166 "10"
                  Whitespace@166..183 "\n                "
                  ExprStmt@183..194
                    ReturnExpr@183..194
                      ReturnKw@183..189 "return"
                      Whitespace@189..190 " "
                      ParenExpr@190..194
                        LParen@190..191 "("
                        Literal@191..193
                          Integer@191..193 "10"
                        RParen@193..194 ")"
                  Whitespace@194..211 "\n                "
                  ExprStmt@211..227
                    ReturnExpr@211..227
                      ReturnKw@211..217 "return"
                      Whitespace@217..218 " "
                      ParenExpr@218..227
                        LParen@218..219 "("
                        BinaryExpr@219..226
                          Literal@219..221
                            Integer@219..221 "10"
                          Whitespace@221..222 " "
                          Plus@222..223 "+"
                          Whitespace@223..224 " "
                          Literal@224..226
                            Integer@224..226 "20"
                        RParen@226..227 ")"
                  Whitespace@227..244 "\n                "
                  ExprStmt@244..252
                    ReturnExpr@244..252
                      ReturnKw@244..250 "return"
                      Whitespace@250..251 " "
                      PathExpr@251..252
                        Path@251..252
                          PathSegment@251..252
                            Ident@251..252 "a"
                  Whitespace@252..269 "\n                "
                  ExprStmt@269..302
                    ReturnExpr@269..302
                      ReturnKw@269..275 "return"
                      Whitespace@275..276 " "
                      IfExpr@276..302
                        IfKw@276..278 "if"
                        Whitespace@278..279 " "
                        Literal@279..283
                          TrueKw@279..283 "true"
                        Whitespace@283..284 " "
                        BlockExpr@284..290
                          LCurly@284..285 "{"
                          Whitespace@285..286 " "
                          ExprStmt@286..288
                            Literal@286..288
                              Integer@286..288 "10"
                          Whitespace@288..289 " "
                          RCurly@289..290 "}"
                        Whitespace@290..291 " "
                        ElseKw@291..295 "else"
                        Whitespace@295..296 " "
                        BlockExpr@296..302
                          LCurly@296..297 "{"
                          Whitespace@297..298 " "
                          ExprStmt@298..300
                            Literal@298..300
                              Integer@298..300 "20"
                          Whitespace@300..301 " "
                          RCurly@301..302 "}"
                  Whitespace@302..319 "\n                "
                  ExprStmt@319..332
                    ReturnExpr@319..332
                      ReturnKw@319..325 "return"
                      Whitespace@325..326 " "
                      BlockExpr@326..332
                        LCurly@326..327 "{"
                        Whitespace@327..328 " "
                        ExprStmt@328..330
                          Literal@328..330
                            Integer@328..330 "10"
                        Whitespace@330..331 " "
                        RCurly@331..332 "}"
                  Whitespace@332..349 "\n                "
                  ExprStmt@349..365
                    ReturnExpr@349..365
                      ReturnKw@349..355 "return"
                      Whitespace@355..356 " "
                      ReturnExpr@356..365
                        ReturnKw@356..362 "return"
                        Whitespace@362..363 " "
                        Literal@363..365
                          Integer@363..365 "10"
                  Whitespace@365..378 "\n            "
            "#]],
        );
    }

    #[test]
    fn parse_path() {
        check_debug_tree_in_block(
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
        check_debug_tree_in_block(
            "a::b::c()",
            expect![[r#"
                SourceFile@0..9
                  ExprStmt@0..9
                    CallExpr@0..9
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
                      ArgList@7..9
                        LParen@7..8 "("
                        RParen@8..9 ")"
            "#]],
        );
    }

    #[test]
    fn parse_path_call_with_params() {
        check_debug_tree_in_block(
            "a::b::c(e, f, g)",
            expect![[r#"
                SourceFile@0..16
                  ExprStmt@0..16
                    CallExpr@0..16
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

    #[test]
    fn parse_loop() {
        check_debug_tree_in_block(
            "loop { 10; 20; 30 }",
            expect![[r#"
                SourceFile@0..19
                  ExprStmt@0..19
                    LoopExpr@0..19
                      LoopKw@0..4 "loop"
                      Whitespace@4..5 " "
                      BlockExpr@5..19
                        LCurly@5..6 "{"
                        Whitespace@6..7 " "
                        ExprStmt@7..10
                          Literal@7..9
                            Integer@7..9 "10"
                          Semicolon@9..10 ";"
                        Whitespace@10..11 " "
                        ExprStmt@11..14
                          Literal@11..13
                            Integer@11..13 "20"
                          Semicolon@13..14 ";"
                        Whitespace@14..15 " "
                        ExprStmt@15..17
                          Literal@15..17
                            Integer@15..17 "30"
                        Whitespace@17..18 " "
                        RCurly@18..19 "}"
            "#]],
        );
    }

    #[test]
    fn parse_continue() {
        check_debug_tree_in_block(
            "loop { continue; }",
            expect![[r#"
                SourceFile@0..18
                  ExprStmt@0..18
                    LoopExpr@0..18
                      LoopKw@0..4 "loop"
                      Whitespace@4..5 " "
                      BlockExpr@5..18
                        LCurly@5..6 "{"
                        Whitespace@6..7 " "
                        ExprStmt@7..16
                          ContinueExpr@7..15
                            ContinueKw@7..15 "continue"
                          Semicolon@15..16 ";"
                        Whitespace@16..17 " "
                        RCurly@17..18 "}"
            "#]],
        );
    }

    #[test]
    fn parse_break() {
        check_debug_tree_in_block(
            "loop { break; }",
            expect![[r#"
                SourceFile@0..15
                  ExprStmt@0..15
                    LoopExpr@0..15
                      LoopKw@0..4 "loop"
                      Whitespace@4..5 " "
                      BlockExpr@5..15
                        LCurly@5..6 "{"
                        Whitespace@6..7 " "
                        ExprStmt@7..13
                          BreakExpr@7..12
                            BreakKw@7..12 "break"
                          Semicolon@12..13 ";"
                        Whitespace@13..14 " "
                        RCurly@14..15 "}"
            "#]],
        );
    }

    #[test]
    fn parse_break_with_expr() {
        check_debug_tree_in_block(
            "loop { break 10; }",
            expect![[r#"
                SourceFile@0..18
                  ExprStmt@0..18
                    LoopExpr@0..18
                      LoopKw@0..4 "loop"
                      Whitespace@4..5 " "
                      BlockExpr@5..18
                        LCurly@5..6 "{"
                        Whitespace@6..7 " "
                        ExprStmt@7..16
                          BreakExpr@7..15
                            BreakKw@7..12 "break"
                            Whitespace@12..13 " "
                            Literal@13..15
                              Integer@13..15 "10"
                          Semicolon@15..16 ";"
                        Whitespace@16..17 " "
                        RCurly@17..18 "}"
            "#]],
        );
    }

    #[test]
    fn parse_while() {
        check_debug_tree_in_block(
            "while true { 10; 20; 30 }",
            expect![[r#"
                SourceFile@0..25
                  ExprStmt@0..25
                    WhileExpr@0..25
                      WhileKw@0..5 "while"
                      Whitespace@5..6 " "
                      Literal@6..10
                        TrueKw@6..10 "true"
                      Whitespace@10..11 " "
                      BlockExpr@11..25
                        LCurly@11..12 "{"
                        Whitespace@12..13 " "
                        ExprStmt@13..16
                          Literal@13..15
                            Integer@13..15 "10"
                          Semicolon@15..16 ";"
                        Whitespace@16..17 " "
                        ExprStmt@17..20
                          Literal@17..19
                            Integer@17..19 "20"
                          Semicolon@19..20 ";"
                        Whitespace@20..21 " "
                        ExprStmt@21..23
                          Literal@21..23
                            Integer@21..23 "30"
                        Whitespace@23..24 " "
                        RCurly@24..25 "}"
            "#]],
        );
    }

    #[test]
    fn parse_while_empty_block() {
        check_debug_tree_in_block(
            "while true { }",
            expect![[r#"
                SourceFile@0..14
                  ExprStmt@0..14
                    WhileExpr@0..14
                      WhileKw@0..5 "while"
                      Whitespace@5..6 " "
                      Literal@6..10
                        TrueKw@6..10 "true"
                      Whitespace@10..11 " "
                      BlockExpr@11..14
                        LCurly@11..12 "{"
                        Whitespace@12..13 " "
                        RCurly@13..14 "}"
            "#]],
        );
    }

    #[test]
    fn parse_while_loss_block() {
        check_debug_tree_in_block(
            "while true",
            expect![[r#"
                SourceFile@0..10
                  ExprStmt@0..10
                    WhileExpr@0..10
                      WhileKw@0..5 "while"
                      Whitespace@5..6 " "
                      Literal@6..10
                        TrueKw@6..10 "true"
            "#]],
        );
    }

    #[test]
    fn parse_while_condition_variable() {
        check_debug_tree_in_block(
            "let a = true; while a { 10 }",
            expect![[r#"
                SourceFile@0..28
                  Let@0..13
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "a"
                    Whitespace@5..6 " "
                    Eq@6..7 "="
                    Whitespace@7..8 " "
                    Literal@8..12
                      TrueKw@8..12 "true"
                    Semicolon@12..13 ";"
                  Whitespace@13..14 " "
                  ExprStmt@14..28
                    WhileExpr@14..28
                      WhileKw@14..19 "while"
                      Whitespace@19..20 " "
                      PathExpr@20..21
                        Path@20..21
                          PathSegment@20..21
                            Ident@20..21 "a"
                      Whitespace@21..22 " "
                      BlockExpr@22..28
                        LCurly@22..23 "{"
                        Whitespace@23..24 " "
                        ExprStmt@24..26
                          Literal@24..26
                            Integer@24..26 "10"
                        Whitespace@26..27 " "
                        RCurly@27..28 "}"
            "#]],
        );
    }

    #[test]
    fn parse_while_cond_block() {
        check_debug_tree_in_block(
            "while { 10; true } { 20 }",
            expect![[r#"
                SourceFile@0..25
                  ExprStmt@0..25
                    WhileExpr@0..25
                      WhileKw@0..5 "while"
                      Whitespace@5..6 " "
                      BlockExpr@6..18
                        LCurly@6..7 "{"
                        Whitespace@7..8 " "
                        ExprStmt@8..11
                          Literal@8..10
                            Integer@8..10 "10"
                          Semicolon@10..11 ";"
                        Whitespace@11..12 " "
                        ExprStmt@12..16
                          Literal@12..16
                            TrueKw@12..16 "true"
                        Whitespace@16..17 " "
                        RCurly@17..18 "}"
                      Whitespace@18..19 " "
                      BlockExpr@19..25
                        LCurly@19..20 "{"
                        Whitespace@20..21 " "
                        ExprStmt@21..23
                          Literal@21..23
                            Integer@21..23 "20"
                        Whitespace@23..24 " "
                        RCurly@24..25 "}"
            "#]],
        );
    }

    #[test]
    fn parse_record_expr() {
        check_debug_tree_in_block(
            "let point = Point {};",
            expect![[r#"
                SourceFile@0..21
                  Let@0..21
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..9 "point"
                    Whitespace@9..10 " "
                    Eq@10..11 "="
                    Whitespace@11..12 " "
                    RecordExpr@12..20
                      Path@12..17
                        PathSegment@12..17
                          Ident@12..17 "Point"
                      Whitespace@17..18 " "
                      RecordFieldListExpr@18..20
                        LCurly@18..19 "{"
                        RCurly@19..20 "}"
                    Semicolon@20..21 ";"
            "#]],
        );

        check_debug_tree_in_block(
            "let point = Point { x: 10 };",
            expect![[r#"
                SourceFile@0..28
                  Let@0..28
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..9 "point"
                    Whitespace@9..10 " "
                    Eq@10..11 "="
                    Whitespace@11..12 " "
                    RecordExpr@12..27
                      Path@12..17
                        PathSegment@12..17
                          Ident@12..17 "Point"
                      Whitespace@17..18 " "
                      RecordFieldListExpr@18..27
                        LCurly@18..19 "{"
                        Whitespace@19..20 " "
                        RecordFieldExpr@20..25
                          Ident@20..21 "x"
                          Colon@21..22 ":"
                          Whitespace@22..23 " "
                          Literal@23..25
                            Integer@23..25 "10"
                        Whitespace@25..26 " "
                        RCurly@26..27 "}"
                    Semicolon@27..28 ";"
            "#]],
        );

        check_debug_tree_in_block(
            "let point = Point { x: 10 ,};",
            expect![[r#"
                SourceFile@0..29
                  Let@0..29
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..9 "point"
                    Whitespace@9..10 " "
                    Eq@10..11 "="
                    Whitespace@11..12 " "
                    RecordExpr@12..28
                      Path@12..17
                        PathSegment@12..17
                          Ident@12..17 "Point"
                      Whitespace@17..18 " "
                      RecordFieldListExpr@18..28
                        LCurly@18..19 "{"
                        Whitespace@19..20 " "
                        RecordFieldExpr@20..25
                          Ident@20..21 "x"
                          Colon@21..22 ":"
                          Whitespace@22..23 " "
                          Literal@23..25
                            Integer@23..25 "10"
                        Whitespace@25..26 " "
                        Comma@26..27 ","
                        RCurly@27..28 "}"
                    Semicolon@28..29 ";"
            "#]],
        );

        check_debug_tree_in_block(
            "let point = Point { x: 10, y: 20 };",
            expect![[r#"
                SourceFile@0..35
                  Let@0..35
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..9 "point"
                    Whitespace@9..10 " "
                    Eq@10..11 "="
                    Whitespace@11..12 " "
                    RecordExpr@12..34
                      Path@12..17
                        PathSegment@12..17
                          Ident@12..17 "Point"
                      Whitespace@17..18 " "
                      RecordFieldListExpr@18..34
                        LCurly@18..19 "{"
                        Whitespace@19..20 " "
                        RecordFieldExpr@20..25
                          Ident@20..21 "x"
                          Colon@21..22 ":"
                          Whitespace@22..23 " "
                          Literal@23..25
                            Integer@23..25 "10"
                        Comma@25..26 ","
                        Whitespace@26..27 " "
                        RecordFieldExpr@27..32
                          Ident@27..28 "y"
                          Colon@28..29 ":"
                          Whitespace@29..30 " "
                          Literal@30..32
                            Integer@30..32 "20"
                        Whitespace@32..33 " "
                        RCurly@33..34 "}"
                    Semicolon@34..35 ";"
            "#]],
        );

        check_debug_tree_in_block(
            "let point = Point { x: 10, y: 20, };",
            expect![[r#"
                SourceFile@0..36
                  Let@0..36
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..9 "point"
                    Whitespace@9..10 " "
                    Eq@10..11 "="
                    Whitespace@11..12 " "
                    RecordExpr@12..35
                      Path@12..17
                        PathSegment@12..17
                          Ident@12..17 "Point"
                      Whitespace@17..18 " "
                      RecordFieldListExpr@18..35
                        LCurly@18..19 "{"
                        Whitespace@19..20 " "
                        RecordFieldExpr@20..25
                          Ident@20..21 "x"
                          Colon@21..22 ":"
                          Whitespace@22..23 " "
                          Literal@23..25
                            Integer@23..25 "10"
                        Comma@25..26 ","
                        Whitespace@26..27 " "
                        RecordFieldExpr@27..32
                          Ident@27..28 "y"
                          Colon@28..29 ":"
                          Whitespace@29..30 " "
                          Literal@30..32
                            Integer@30..32 "20"
                        Comma@32..33 ","
                        Whitespace@33..34 " "
                        RCurly@34..35 "}"
                    Semicolon@35..36 ";"
            "#]],
        );
    }

    #[test]
    fn parse_field_expr() {
        check_debug_tree_in_block(
            "value.foo_bar;",
            expect![[r#"
                SourceFile@0..14
                  ExprStmt@0..14
                    FieldExpr@0..13
                      PathExpr@0..5
                        Path@0..5
                          PathSegment@0..5
                            Ident@0..5 "value"
                      Dot@5..6 "."
                      Ident@6..13 "foo_bar"
                    Semicolon@13..14 ";"
            "#]],
        );

        check_debug_tree_in_block(
            "scope::value.foo_bar;",
            expect![[r#"
                SourceFile@0..21
                  ExprStmt@0..21
                    FieldExpr@0..20
                      PathExpr@0..12
                        Path@0..12
                          PathSegment@0..5
                            Ident@0..5 "scope"
                          Colon2@5..7 "::"
                          PathSegment@7..12
                            Ident@7..12 "value"
                      Dot@12..13 "."
                      Ident@13..20 "foo_bar"
                    Semicolon@20..21 ";"
            "#]],
        );

        check_debug_tree_in_block(
            "let f = value.foo_bar;",
            expect![[r#"
                SourceFile@0..22
                  Let@0..22
                    LetKw@0..3 "let"
                    Whitespace@3..4 " "
                    Ident@4..5 "f"
                    Whitespace@5..6 " "
                    Eq@6..7 "="
                    Whitespace@7..8 " "
                    FieldExpr@8..21
                      PathExpr@8..13
                        Path@8..13
                          PathSegment@8..13
                            Ident@8..13 "value"
                      Dot@13..14 "."
                      Ident@14..21 "foo_bar"
                    Semicolon@21..22 ";"
            "#]],
        );
    }

    #[test]
    fn field_expr_in_semicolon_is_error() {
        check_debug_tree_in_block(
            "value;.foo_bar",
            expect![[r#"
                SourceFile@0..14
                  ExprStmt@0..6
                    PathExpr@0..5
                      Path@0..5
                        PathSegment@0..5
                          Ident@0..5 "value"
                    Semicolon@5..6 ";"
                  ExprStmt@6..7
                    Error@6..7
                      Dot@6..7 "."
                  ExprStmt@7..14
                    PathExpr@7..14
                      Path@7..14
                        PathSegment@7..14
                          Ident@7..14 "foo_bar"
                error at 6..7: expected 'let', 'fn', 'struct', 'mod', integerLiteral, charLiteral, stringLiteral, 'true', 'false', identifier, '-', '!', '(', '{', 'if', 'return', 'loop', 'continue', 'break' or 'while', but found '.'
            "#]],
        );

        check_debug_tree_in_block(
            "value.;foo_bar",
            expect![[r#"
                SourceFile@0..14
                  ExprStmt@0..7
                    FieldExpr@0..7
                      PathExpr@0..5
                        Path@0..5
                          PathSegment@0..5
                            Ident@0..5 "value"
                      Dot@5..6 "."
                      Error@6..7
                        Semicolon@6..7 ";"
                  ExprStmt@7..14
                    PathExpr@7..14
                      Path@7..14
                        PathSegment@7..14
                          Ident@7..14 "foo_bar"
                error at 6..7: expected identifier, but found ';'
            "#]],
        );
    }
}
