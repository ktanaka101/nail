use crate::parser::ParserError;
use syntax::SyntaxKind;

#[derive(Debug, PartialEq, Eq)]
pub enum Event {
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    AddToken,
    FinishNode,
    Error(ParserError),
    Placeholder,
}
