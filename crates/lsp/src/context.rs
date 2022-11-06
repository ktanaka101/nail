use std::collections::HashMap;
use std::fs;
use std::path;

use anyhow::Result;
use lsp_types::Url;

#[derive(Debug, Default)]
pub struct Context {
    analyses: HashMap<Url, Analysis>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_file(&mut self, uri: Url) -> Result<()> {
        let path = path::Path::new(uri.path());
        let content = fs::read_to_string(path)?;

        self.analyses
            .insert(uri.clone(), Analysis::new(uri, content));

        Ok(())
    }
}

#[derive(Debug)]
pub struct Analysis {
    pub uri: Url,
    pub content: String,
}

impl Analysis {
    pub fn new(uri: Url, content: String) -> Self {
        Self { uri, content }
    }
}
