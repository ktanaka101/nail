mod preludes {
    pub use std::{
        convert::{TryFrom, TryInto},
        fmt,
        fmt::{Display, Formatter},
    };

    pub use anyhow::Result;
}

pub mod ast_parser;
pub mod hir;
pub mod hir_parser;
pub mod lexer;
pub mod llvm;
pub mod token;
pub mod type_checker;

pub mod parser {
    pub use parser::parse;
}
