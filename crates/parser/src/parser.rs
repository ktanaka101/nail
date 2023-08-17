pub(crate) mod marker;

mod parse_error;

use indexmap::IndexSet;
use lexer::{Token, TokenKind};
use marker::NodeMarker;
pub use parse_error::{ParseError, ParserError, TokenError};
use syntax::SyntaxKind;

use crate::{event::Event, grammar, source::Source};

/// CSTを構築するためのパーサー
///
/// CST構築のために以下の概念があります。
/// - Node
/// - ErrorNode
/// - Token
///
/// Nodeは小要素としてNode/ErrorNode/Tokenを0件以上持ちます。
/// ErrorNodeは小要素としてTokenを0件以上持ちます。
/// Tokenは小要素を持ちません。(NodeとTokenは、一般的なNodeとLeafNodeの関係です)
/// [SyntaxKind::Error]が、ErrorNodeの概念です。
///
/// Error表現は、[SyntaxKind::Error]と[Event::Error]の2種類があるため、混在に注意してください。
/// [SyntaxKind::Error]がErrorNodeの概念で、CST中の構造として表現されます。
/// [Event::Error]はパース中に発生したエラーを表現するためのイベントで、CST中の構造には現れません。
pub(crate) struct Parser<'l, 'input> {
    source: Source<'l, 'input>,
    events: Vec<Event>,
    expected_kinds: IndexSet<TokenKind>,
}

/// トップレベルで回復可能なトークンの集合
pub(crate) const TOPLEVEL_RECOVERY_SET: [TokenKind; 2] = [TokenKind::FnKw, TokenKind::ModKw];
/// ブロック内で回復可能なトークンの集合
pub(crate) const BLOCK_RECOVERY_SET: [TokenKind; 3] =
    [TokenKind::LetKw, TokenKind::FnKw, TokenKind::ModKw];

impl<'l, 'input> Parser<'l, 'input> {
    /// CSTを構築するパーサーを作成します。
    pub(crate) fn new(source: Source<'l, 'input>) -> Self {
        Self {
            source,
            events: vec![],
            expected_kinds: IndexSet::new(),
        }
    }

    /// パースします。
    pub(crate) fn parse(mut self) -> Vec<Event> {
        grammar::parse_source_file(&mut self);
        self.events
    }

    /// ブロックコンテキストでパースを行います。
    #[cfg(test)]
    pub(crate) fn parse_in_block(mut self) -> Vec<Event> {
        grammar::parse_in_block(&mut self);
        self.events
    }

    /// Node開始イベントを追加し、Nodeマーカーを返します。
    pub(crate) fn start(&mut self) -> NodeMarker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);

        NodeMarker::new(pos)
    }

    /// トークンを1つ進め、トークン追加イベントを追加します。
    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.source.next_token().unwrap();
        self.events.push(Event::AddToken)
    }

    /// 指定したトークン種別`kind`が、現在のカーソルが示すトークン種別と一致するかを返します。
    ///
    /// この関数を呼び出すと、指定した`kind`が期待するトークンリストに追加されます。
    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.insert(kind);
        match (self.peek(), kind) {
            (Some(TokenKind::CharLiteral(_)), TokenKind::CharLiteral(_)) => true,
            (peek_kind, kind) => peek_kind == Some(kind),
        }
    }

    /// 指定したトークン種別一覧`set`のいずれかが、現在のカーソルが示すトークン種別と一致するかを返します。
    pub(crate) fn at_set(&mut self, set: &[TokenKind]) -> bool {
        for kind in set {
            if self.at(*kind) {
                return true;
            }
        }
        false
    }

    /// 指定したトークン種別`kind`が、現在のカーソルが示すトークン種別と一致した場合にトークンを消費します。
    ///
    /// この関数はブロックコンテキストで呼び出すことを想定しています。
    /// トークン種別が一致しない場合は、エラーイベントを追加しトークンを消費します。
    /// ただし、ブロック内で復旧可能なトークンであれば、エラーイベントは追加されず、トークンも消費されません。
    pub(crate) fn expect_on_block(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error_with_recovery_set_only_default_on_block();
        }
    }

    /// 指定したトークン種別`kind`が、現在のカーソルが示すトークン種別と一致した場合にトークンを消費します。
    ///
    /// この関数はブロックコンテキストで呼び出すことを想定しています。
    /// トークン種別が一致しない場合は、エラーイベントを追加しトークンを消費します。
    /// ただし、復旧可能なトークン一覧`recovery_set`に含まれるトークンであれば、エラーイベントは追加されず、トークンも消費されません。
    ///
    /// [Self::expect_on_block]とブロックコンテキストにおける復旧トークンで判定されることは同じですが、
    /// 追加で個別の復旧トークンを指定することができます。
    /// これは例えば、`let = 10;`の場合に`=`を復旧トークンとして指定することで、
    /// `let = 10;`までを変数定義ノードとして扱うことができます。
    /// 仮に指定できない場合、`let`のみが変数定義ノードとして扱うことになります。
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

    /// 指定したトークン種別`kind`が、現在のカーソルが示すトークン種別と一致した場合にトークンを消費します。
    ///
    /// トークン種別が一致しない場合は、エラーイベントを追加しトークンを消費します。
    /// ただし、復旧可能なトークンであれば、エラーイベントは追加されず、トークンも消費されません。
    ///
    /// この関数は、トップレベルコンテキスト、ブロックコンテキストのデフォルトの復旧トークンを使用したくない場合に使用します。
    /// 復旧トークンは指定した`recovery_set`のみとなります。
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

    /// 現在のトークンでエラーイベントを追加します。
    /// `expected_kinds`は期待されるトークンを指定します。特にない場合は空のリストを指定します。
    ///
    /// この関数を呼び出してもトークンは消費されません。そのため、ノードに[SyntaxKind::Error]は追加されません。
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

    /// トップレベルコンテキストの復旧トークンで、現在のトークンでエラーイベントを追加し、[SyntaxKind::Error]にトークンを消費します。
    ///
    /// この関数はトークンを消費します。
    /// ただし、復旧可能なトークンであれば、エラーイベントは追加されず、トークンも消費されません。
    pub(crate) fn error_with_recovery_set_only_default_on_toplevel(&mut self) {
        self.error_with_recovery_set_no_default(&TOPLEVEL_RECOVERY_SET);
    }

    /// ブロックコンテキストの復旧トークンで、現在のトークンでエラーイベントを追加し、[SyntaxKind::Error]にトークンを消費します。
    ///
    /// この関数はトークンを消費します。
    /// ただし、復旧可能なトークンであれば、エラーイベントは追加されず、トークンも消費されません。
    pub(crate) fn error_with_recovery_set_only_default_on_block(&mut self) {
        self.error_with_recovery_set_no_default(&BLOCK_RECOVERY_SET);
    }

    /// ブロックコンテキストの復旧トークンで、現在のトークンでエラーイベントを追加し、[SyntaxKind::Error]にトークンを消費します。
    ///
    /// 指定した`recovery_set`を復旧トークンとして、ブロックコンテキストの復旧トークンとマージします。
    /// この関数はトークンを消費します。
    /// ただし、復旧可能なトークンであれば、エラーイベントは追加されず、トークンも消費されません。
    pub(crate) fn error_with_recovery_set_default_on_block(&mut self, recovery_set: &[TokenKind]) {
        self.error_with_recovery_set_no_default(&[recovery_set, &BLOCK_RECOVERY_SET].concat());
    }

    /// デフォルトの復旧トークンなしで現在のトークンでエラーイベントを追加し、[SyntaxKind::Error]にトークンを消費します。
    ///
    /// 指定した`recovery_set`のみを復旧トークンとして扱います。
    /// この関数はトークンを消費します。
    /// ただし、復旧可能なトークンであれば、エラーイベントは追加されず、トークンも消費されません。
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

    /// 現在のカーソルが末尾かどうかを返します。
    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    /// 現在のトークンが指定した`set`に含まれているかどうかを返します。
    pub(crate) fn at_set_no_expected(&mut self, set: &[TokenKind]) -> bool {
        self.peek().map_or(false, |k| set.contains(&k))
    }

    /// 現在のトークンを返します。
    ///
    /// 現在のトークンが存在しない場合は`None`を返します。(最後まで進んだ等)
    pub(crate) fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }
}
