mod expr;
mod stmt;
mod toplevel;

use lexer::TokenKind;
use syntax::SyntaxKind;

use crate::parser::{marker::CompletedNodeMarker, Parser};

/// ルートのソースコードをパースします。
pub(crate) fn parse_source_file(parser: &mut Parser) -> CompletedNodeMarker {
    let marker = parser.start();
    while !parser.at_end() {
        toplevel::parse_stmt_on_toplevel(parser);
    }

    marker.complete(parser, SyntaxKind::SourceFile)
}

/// ブロック内コンテキストとしてパースします。
///
/// テスト用に定義されています。
#[cfg(test)]
pub(crate) fn parse_in_block(parser: &mut Parser) -> CompletedNodeMarker {
    let marker = parser.start();
    while !parser.at_end() {
        stmt::parse_stmt_on_block(parser);
    }

    marker.complete(parser, SyntaxKind::SourceFile)
}

/// パスをパースします。
pub(crate) fn parse_path(parser: &mut Parser, recovery_set: &[TokenKind]) -> CompletedNodeMarker {
    assert!(parser.at(TokenKind::Ident));

    let marker = parser.start();

    parse_path_segment(parser, recovery_set);
    while parser.at(TokenKind::Colon2) {
        parser.bump();
        parse_path_segment(parser, recovery_set);
    }

    marker.complete(parser, SyntaxKind::Path)
}

/// パスのセグメントをパースします。
pub(crate) fn parse_path_segment(
    parser: &mut Parser,
    recovery_set: &[TokenKind],
) -> CompletedNodeMarker {
    let marker = parser.start();

    parser.expect_with_recovery_set_no_default(TokenKind::Ident, recovery_set);

    marker.complete(parser, SyntaxKind::PathSegment)
}
