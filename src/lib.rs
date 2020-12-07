mod preludes {
    pub use anyhow::Result;
    pub use std::convert::{TryFrom, TryInto};
    pub use std::fmt;
    pub use std::fmt::{Display, Formatter};
}

pub mod evaluator;
pub mod lexer;
pub mod llvm;
pub mod normalizer;
pub mod parser;
pub mod repl;
pub mod type_checker;
