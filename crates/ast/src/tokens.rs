mod operators;
pub use operators::*;
use syntax::{SyntaxKind, SyntaxToken};

use crate::ast_node::{Ast, AstToken};

type StdString = std::string::String;

/// ASTトークンを定義します。
///
/// # Example
///
/// このマクロは、`def_ast_token(BinaryOp);`と呼び出すと以下のように展開されます。
/// ```ignore
/// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// pub struct Integer {
///     syntax: SyntaxToken,
/// }
///
/// impl Ast for Integer {}
/// impl AstToken for Integer {
///     fn can_cast(kind: SyntaxKind) -> bool {
///         kind == SyntaxKind::Integer
///     }
///
///     fn cast(syntax: SyntaxToken) -> Option<Self> {
///         if Self::can_cast(syntax.kind()) {
///             Some(Self { syntax })
///         } else {
///             None
///         }
///     }
///
///     fn syntax(&self) -> &SyntaxToken {
///         &self.syntax
///     }
/// }
/// ```
macro_rules! def_ast_token {
    ($(#[$meta:meta])* $kind:ident) => {
        /// ASTトークン
        $(#[$meta])*
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $kind {
            syntax: SyntaxToken,
        }

        impl Ast for $kind {}
        impl AstToken for $kind {
            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::$kind
            }

            fn cast(syntax: SyntaxToken) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self { syntax })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxToken {
                &self.syntax
            }
        }
    };
}
pub(in crate::tokens) use def_ast_token;

def_ast_token!(
    /// 整数リテラル
    Integer
);
impl Integer {
    /// 整数値を返します。
    /// 例: `123`の場合、`123`を返します。
    pub fn value(&self) -> Option<u64> {
        self.syntax.text().parse().ok()
    }
}

def_ast_token!(
    /// 文字列リテラル
    String
);
impl String {
    /// 文字列を返します。
    /// 例: `"Hello, world!"`の場合、`Hello, world!`を返します。
    pub fn value(&self) -> Option<StdString> {
        let text = self.syntax.text();
        let text = text.trim_matches('"');
        Some(text.to_string())
    }
}

def_ast_token!(
    /// 文字リテラル
    Char
);
impl Char {
    /// 文字リテラル内の文字部分を返します。
    /// 例: `'a'`の場合、`a`を返します。
    pub fn value(&self) -> Option<char> {
        let text = self.syntax.text();
        let text = text.trim_matches('\'');
        text.chars().next()
    }
}

/// 真偽値リテラル
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Bool {
    syntax: SyntaxToken,
}
impl Ast for Bool {}
impl AstToken for Bool {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, SyntaxKind::TrueKw | SyntaxKind::FalseKw)
    }

    fn cast(syntax: SyntaxToken) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
}
impl Bool {
    /// 真偽値を返します。
    pub fn value(&self) -> Option<bool> {
        match self.syntax.kind() {
            SyntaxKind::TrueKw => Some(true),
            SyntaxKind::FalseKw => Some(false),
            _ => None,
        }
    }
}

def_ast_token!(
    /// 識別子
    Ident
);
impl Ident {
    /// 識別子の名前を返します。
    /// 例: `x`、`foo`、`bar`など
    pub fn name(&self) -> &str {
        self.syntax.text()
    }
}

def_ast_token!(Semicolon);
