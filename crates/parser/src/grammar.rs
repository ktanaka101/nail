mod expr;
mod stmt;

use syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

pub(crate) fn root(parser: &mut Parser) -> CompletedMarker {
    let marker = parser.start();
    stmt::stmt(parser);

    marker.complete(parser, SyntaxKind::Root)
}
