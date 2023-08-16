use lexer::{Token, TokenKind};
use text_size::TextRange;

/// パース元のトークン列を表す。
pub(crate) struct Source<'l, 'input> {
    tokens: &'l [Token<'input>],
    cursor: usize,
}

impl<'l, 'input> Source<'l, 'input> {
    /// パース元のトークン列を表す[Source]を作成する。
    pub(crate) fn new(tokens: &'l [Token<'input>]) -> Self {
        Self { tokens, cursor: 0 }
    }

    /// トークンを返し、トークン位置を指すカーソルを次へ進めます。
    ///
    /// トリビアのトークンを飛ばすようにカーソルを進めてから返すトークンが決まります。
    pub(crate) fn next_token(&mut self) -> Option<&'l Token<'input>> {
        self.eat_trivia();

        let token = self.tokens.get(self.cursor)?;
        self.cursor += 1;

        Some(token)
    }

    /// トークン種別を返します。
    ///
    /// トリビアのトークンを飛ばすようにカーソルを進めてから返すトークンが決まります。
    pub(crate) fn peek_kind(&mut self) -> Option<TokenKind> {
        self.eat_trivia();
        self.peek_kind_raw()
    }

    /// トークンを返します。
    ///
    /// トリビアのトークンを飛ばすようにカーソルを進めてから返すトークンが決まります。
    pub(crate) fn peek_token(&mut self) -> Option<&Token> {
        self.eat_trivia();
        self.peek_token_raw()
    }

    /// 最後のトークンの範囲を返します。
    pub(crate) fn last_token_range(&self) -> Option<TextRange> {
        self.tokens.last().map(|Token { range, .. }| *range)
    }

    /// トリビアのトークンを飛ばすようにカーソルを進めます。
    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1;
        }
    }

    /// カーソルがトリビアのトークンを指しているかどうかを返します。
    fn at_trivia(&self) -> bool {
        self.peek_kind_raw().map_or(false, TokenKind::is_trivia)
    }

    /// カーソルが指しているトークン種別を返します。
    fn peek_kind_raw(&self) -> Option<TokenKind> {
        self.peek_token_raw().map(|Token { kind, .. }| *kind)
    }

    /// カーソルが指しているトークンを返します。
    fn peek_token_raw(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }
}
