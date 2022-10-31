use crate::lexer2::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Event<'a> {
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    AddToken {
        kind: SyntaxKind,
        text: &'a str,
    },
    FinishNode,
    Placeholder,
}
