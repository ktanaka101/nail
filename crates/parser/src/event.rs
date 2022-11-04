use crate::parser::ParserError;
use syntax::SyntaxKind;

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
}
