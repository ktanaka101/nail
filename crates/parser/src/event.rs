use syntax::SyntaxKind;

use crate::parser::ParserError;

#[derive(Debug, PartialEq)]
pub(crate) enum Event {
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    AddToken,
    FinishNode,
    Error(ParserError),
    Placeholder,
    Ignore,
}
