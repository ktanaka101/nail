use crate::lexer2::SyntaxKind;

#[derive(Debug, Clone)]
pub(super) enum Event<'a> {
    StartNode { kind: SyntaxKind },
    StartNodeAt { kind: SyntaxKind, checkpoint: usize },
    AddToken { kind: SyntaxKind, text: &'a str },
    FinishNode,
}
