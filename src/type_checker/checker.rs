use anyhow::Result;

use crate::ast_parser::ast;

#[derive(Default)]
pub struct Checker {}

#[derive(thiserror::Error, Debug, Clone)]
pub enum Error {}

impl Checker {
    pub fn new() -> Self {
        Checker {}
    }

    pub fn check(&self, _node: &ast::Node) -> Result<()> {
        Ok(())
    }
}
