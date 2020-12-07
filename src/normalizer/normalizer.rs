use anyhow::Result;

use crate::parser::ast;

#[derive(thiserror::Error, Debug, Clone)]
pub enum Error {}

pub struct Normalizer {}

impl Normalizer {
    pub fn new() -> Self {
        Normalizer {}
    }

    pub fn normalize(&self, node: &ast::Node) -> Result<ast::Node> {
        Ok(node.clone())
    }
}
