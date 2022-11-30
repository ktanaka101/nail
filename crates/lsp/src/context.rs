use std::{collections::HashMap, fs, path};

use anyhow::Result;
use lsp_types::{TextDocumentIdentifier, TextDocumentItem, Url, VersionedTextDocumentIdentifier};
use tower_lsp::lsp_types;

use crate::analysis::Analysis;

#[derive(Debug, Default)]
pub struct Context {
    analyses: HashMap<Url, Analysis>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_analysis_from_cache(
        &self,
        text_document: TextDocumentIdentifier,
    ) -> Option<&Analysis> {
        self.analyses.get(&text_document.uri)
    }

    pub fn add_file(&mut self, text_document: TextDocumentItem) -> Result<&Analysis> {
        let analysis = Analysis::new(text_document.uri, text_document.text);

        let uri = analysis.uri.clone();
        self.analyses.insert(uri.clone(), analysis);

        Ok(self.analyses.get(&uri).unwrap())
    }

    pub fn update_file(&mut self, id: &VersionedTextDocumentIdentifier) -> Result<&Analysis> {
        let path = path::Path::new(id.uri.path());
        let content = fs::read_to_string(path)?;
        let analysis = Analysis::new(id.uri.clone(), content);

        self.analyses.insert(id.uri.clone(), analysis);

        Ok(self.analyses.get(&id.uri).unwrap())
    }

    pub fn remove_file(&mut self, text_document: TextDocumentIdentifier) -> Result<()> {
        self.analyses.remove(&text_document.uri);

        Ok(())
    }
}
