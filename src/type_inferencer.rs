use anyhow::Result;

use crate::parser::ast;

#[derive(Default)]
pub struct TypeInferencer {}

#[derive(thiserror::Error, Debug, Clone)]
pub enum Error {}

impl TypeInferencer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn inference(&self, _node: &ast::Node) -> Result<()> {
        Ok(())
    }
}
