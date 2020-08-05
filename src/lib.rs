mod preludes {
    pub use anyhow::Result;
    pub use std::convert::{TryFrom, TryInto};
    pub use std::fmt;
    pub use std::fmt::{Display, Formatter};
}

pub mod evaluator;
pub mod lexer;
pub mod parser;
pub mod repl;
