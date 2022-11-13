use std::sync::Arc;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::analysis::Analysis;
use crate::context::Context;
use crate::semantic_tokens::SEMANTIC_TOKEN_TYPES;

#[derive(Debug, Clone)]
pub struct NailLanguageServer(Arc<tokio::sync::Mutex<Backend>>);

impl NailLanguageServer {
    pub fn new(client: Client) -> Self {
        Self(Arc::new(tokio::sync::Mutex::new(Backend {
            client,
            context: Context::new(),
        })))
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for NailLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.0.lock().await.initialize(params).await
    }

    async fn initialized(&self, params: InitializedParams) {
        self.0.lock().await.initialized(params).await
    }

    async fn shutdown(&self) -> Result<()> {
        self.0.lock().await.shutdown().await
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.0.lock().await.did_open(params).await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.0.lock().await.did_change(params).await
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.0.lock().await.did_close(params).await
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        self.0.lock().await.semantic_tokens_full(params).await
    }
}

#[derive(Debug)]
struct Backend {
    client: Client,
    context: Context,
}

impl Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        self.info("server initialize!").await;
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("nail".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions {
                                    work_done_progress: None,
                                },
                                legend: SemanticTokensLegend {
                                    token_types: SEMANTIC_TOKEN_TYPES.into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(false),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions { id: None },
                        },
                    ),
                ),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: "nail-language-server".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.info("server initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        self.info("server shutdown!").await;
        Ok(())
    }

    async fn did_open(&mut self, params: DidOpenTextDocumentParams) {
        self.info(&format!(
            "server did open! get uri: {}",
            params.text_document.uri
        ))
        .await;
        match self.context.add_file(params.text_document) {
            Ok(analysis) => {
                let diagnostics = get_diagnostics(analysis);
                self.client
                    .publish_diagnostics(analysis.uri.clone(), diagnostics, None)
                    .await;
            }
            Err(e) => self.client.show_message(MessageType::LOG, e).await,
        }
    }

    async fn did_change(&mut self, params: DidChangeTextDocumentParams) {
        self.info(&format!(
            "server did change! get uri: {}",
            params.text_document.uri
        ))
        .await;

        // FIXME: this is a hack to get the uri. Do not read from the file system in Context::upadte_file.
        tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;

        match self.context.update_file(&params.text_document) {
            Ok(analysis) => {
                let diagnostics = get_diagnostics(analysis);
                self.client
                    .publish_diagnostics(analysis.uri.clone(), diagnostics, None)
                    .await;
            }
            Err(e) => self.client.show_message(MessageType::LOG, e).await,
        }
    }

    async fn did_close(&mut self, params: DidCloseTextDocumentParams) {
        self.info(&format!(
            "server did close! get uri: {}",
            params.text_document.uri
        ))
        .await;
        if let Err(e) = self.context.remove_file(params.text_document) {
            self.client.show_message(MessageType::LOG, e).await;
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        self.info(&format!(
            "server semantic tokens full! get uri: {}",
            params.text_document.uri
        ))
        .await;

        if let Some(analysis) = self.context.get_analysis_from_cache(params.text_document) {
            let tokens = analysis.semantic_tokens();
            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })))
        } else {
            Ok(None)
        }
    }

    async fn info(&self, message: &str) {
        self.client.log_message(MessageType::INFO, message).await;
    }
}

fn get_diagnostics(analysis: &Analysis) -> Vec<Diagnostic> {
    let diagnostics = analysis.diagnostics();
    diagnostics
        .iter()
        .map(|diagnostic| {
            Diagnostic::new_simple(
                diagnostic.range(&analysis.line_index),
                diagnostic.display(&analysis.line_index),
            )
        })
        .collect::<Vec<_>>()
}
