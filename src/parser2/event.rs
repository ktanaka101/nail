use crate::lexer2::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Event {
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    AddToken,
    FinishNode,
    Placeholder,
}
