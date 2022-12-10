mod expr;
mod stmt;
mod toplevel;

use syntax::SyntaxKind;

use crate::parser::{marker::CompletedMarker, Parser};

pub(crate) fn source_file(parser: &mut Parser) -> CompletedMarker {
    let marker = parser.start();
    while !parser.at_end() {
        toplevel::parse_stmt_on_toplevel(parser);
    }

    marker.complete(parser, SyntaxKind::SourceFile)
}

#[cfg(test)]
pub(crate) fn in_block(parser: &mut Parser) -> CompletedMarker {
    let marker = parser.start();
    while !parser.at_end() {
        stmt::parse_stmt_on_block(parser);
    }

    marker.complete(parser, SyntaxKind::SourceFile)
}
