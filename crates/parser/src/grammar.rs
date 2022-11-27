mod expr;
mod stmt;

use syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

pub(crate) fn source_file(parser: &mut Parser) -> CompletedMarker {
    let marker = parser.start();
    while !parser.at_end() {
        stmt::parse_stmt(parser);
    }

    marker.complete(parser, SyntaxKind::SourceFile)
}
