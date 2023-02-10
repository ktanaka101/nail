pub(crate) mod marker;

mod parse_error;

use indexmap::IndexSet;
use lexer::{Token, TokenKind};
use marker::Marker;
pub use parse_error::{ParseError, ParserError, TokenError};
use syntax::SyntaxKind;

use crate::{event::Event, grammar, source::Source};

pub(crate) struct Parser<'l, 'input> {
    source: Source<'l, 'input>,
    events: Vec<Event>,
    expected_kinds: IndexSet<TokenKind>,
}

pub(crate) const TOPLEVEL_RECOVERY_SET: [TokenKind; 1] = [TokenKind::FnKw];
pub(crate) const BLOCK_RECOVERY_SET: [TokenKind; 2] = [TokenKind::LetKw, TokenKind::FnKw];

impl<'l, 'input> Parser<'l, 'input> {
    pub(crate) fn new(source: Source<'l, 'input>) -> Self {
        Self {
            source,
            events: vec![],
            expected_kinds: IndexSet::new(),
        }
    }

    pub(crate) fn parse(mut self) -> Vec<Event> {
        grammar::source_file(&mut self);
        self.events
    }

    #[cfg(test)]
    pub(crate) fn parse_in_block(mut self) -> Vec<Event> {
        grammar::in_block(&mut self);
        self.events
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        Marker::new(pos)
    }

    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.source.next_token().unwrap();
        self.events.push(Event::AddToken)
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.insert(kind);
        match (self.peek(), kind) {
            (Some(TokenKind::CharLiteral(_)), TokenKind::CharLiteral(_)) => true,
            (peek_kind, kind) => peek_kind == Some(kind),
        }
    }

    pub(crate) fn at_set(&mut self, set: &[TokenKind]) -> bool {
        for kind in set {
            if self.at(*kind) {
                return true;
            }
        }
        false
    }

    pub(crate) fn expect_on_block(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error_with_recovery_set_only_default_on_block();
        }
    }

    pub(crate) fn expect_with_block_recovery_set(
        &mut self,
        kind: TokenKind,
        recovery_set: &[TokenKind],
    ) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error_with_recovery_set_default_on_block(recovery_set)
        }
    }

    pub(crate) fn expect_with_recovery_set_no_default(
        &mut self,
        kind: TokenKind,
        recovery_set: &[TokenKind],
    ) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error_with_recovery_set_no_default(recovery_set)
        }
    }

    pub(crate) fn error_in_token(&mut self, expected_kinds: Vec<TokenKind>) {
        let current_token = self.source.peek_token();
        let (found, range) = if let Some(Token { kind, range, .. }) = current_token {
            (*kind, *range)
        } else {
            panic!("Bug: called latest token.");
        };

        self.events
            .push(Event::Error(ParserError::TokenError(TokenError {
                expected: expected_kinds,
                actual: found,
                range,
            })));
    }

    pub(crate) fn error_with_recovery_set_only_default_on_toplevel(&mut self) {
        self.error_with_recovery_set_no_default(&TOPLEVEL_RECOVERY_SET);
    }

    pub(crate) fn error_with_recovery_set_only_default_on_block(&mut self) {
        self.error_with_recovery_set_no_default(&BLOCK_RECOVERY_SET);
    }

    pub(crate) fn error_with_recovery_set_default_on_block(&mut self, recovery_set: &[TokenKind]) {
        self.error_with_recovery_set_no_default(&[recovery_set, &BLOCK_RECOVERY_SET].concat());
    }

    pub(crate) fn error_with_recovery_set_no_default(&mut self, recovery_set: &[TokenKind]) {
        let current_token = self.source.peek_token();

        let (found, range) = if let Some(Token { kind, range, .. }) = current_token {
            (Some(*kind), *range)
        } else {
            (None, self.source.last_token_range().unwrap())
        };
        self.events
            .push(Event::Error(ParserError::ParseError(ParseError {
                expected: self.expected_kinds.drain(..).collect(),
                found,
                range,
            })));

        if !self.at_set_no_expected(recovery_set) && !self.at_end() {
            let marker = self.start();
            self.bump();
            marker.complete(self, SyntaxKind::Error);
        }
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    pub(crate) fn at_set_no_expected(&mut self, set: &[TokenKind]) -> bool {
        self.peek().map_or(false, |k| set.contains(&k))
    }

    pub(crate) fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }
}
