mod preludes {
    pub use anyhow::Result;
    pub use std::convert::{TryFrom, TryInto};
    pub use std::fmt;
    pub use std::fmt::{Display, Formatter};
}

pub mod ast_parser;
pub mod hir;
pub mod hir_parser;
pub mod lexer;
pub mod lexer2;
pub mod llvm;
pub mod parser2;
pub mod repl;
pub mod syntax;
pub mod token;
pub mod type_checker;
