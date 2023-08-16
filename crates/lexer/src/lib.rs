//! 字句解析を行うためのクレートです。

#![warn(missing_docs)]

mod token_kind;

use std::ops::Range as StdRange;

use logos::Logos;
use text_size::{TextRange, TextSize};
pub use token_kind::{Token, TokenKind};

/// 字句解析器
pub struct Lexer<'a> {
    inner: logos::Lexer<'a, TokenKind>,
}

impl<'a> Lexer<'a> {
    /// 新しい字句解析器を返します。
    ///
    /// # Parameters
    ///
    /// `input` - 字句解析する文字列を指定します。
    pub fn new(input: &'a str) -> Self {
        Self {
            inner: TokenKind::lexer(input),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();

        let range = {
            let StdRange { start, end } = self.inner.span();
            let start = TextSize::try_from(start).unwrap();
            let end = TextSize::try_from(end).unwrap();

            TextRange::new(start, end)
        };

        Some(Self::Item { kind, text, range })
    }
}
