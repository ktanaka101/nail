mod expr;

use syntax::SyntaxKind;

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

pub(crate) fn root(parser: &mut Parser) -> CompletedMarker {
    let marker = parser.start();
    expr::expr(parser);

    marker.complete(parser, SyntaxKind::Root)
}
