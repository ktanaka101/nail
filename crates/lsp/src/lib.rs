//! Language Server Protocol implementation for Nail.

mod analysis;
// mod language_server;
mod line_index;
mod semantic_tokens;

use std::env;

use lsp_server::Connection;
use lsp_types::{
    DocumentFilter, SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    SemanticTokensRegistrationOptions, SemanticTokensServerCapabilities, ServerCapabilities,
    StaticRegistrationOptions, TextDocumentRegistrationOptions, TextDocumentSyncCapability,
    TextDocumentSyncKind, WorkDoneProgressOptions,
};
use semantic_tokens::SEMANTIC_TOKEN_TYPES;
use serde::de::DeserializeOwned;
use thiserror::Error;

#[derive(Error, Debug)]
enum Error {
    #[error("root_uri is required")]
    RequiredRootUri,
}

/// Run the language server.
pub async fn run_server() -> anyhow::Result<()> {
    tracing::info!("server will start");

    let (connection, io_threads) = Connection::stdio();

    let (initialize_id, initialize_params) = match connection.initialize_start() {
        Ok(it) => it,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };
    tracing::info!("InitializeParams: {}", initialize_params);

    let lsp_types::InitializeParams {
        root_uri,
        capabilities,
        workspace_folders: _,
        initialization_options,
        ..
    } = from_json::<lsp_types::InitializeParams>("InitializeParams", &initialize_params)?;

    let root_dir_path = if let Some(root_uri) = &root_uri {
        if let Ok(root_dir_path) = root_uri.to_file_path() {
            root_dir_path
        } else {
            tracing::info!("root_uri must be a file path.");
            env::current_dir()?
        }
    } else {
        tracing::warn!("root_uri is required");
        return Err(Error::RequiredRootUri.into());
    };

    let config = Config::new(root_dir_path, capabilities);
    if let Some(json) = initialization_options {
        // todo: update config
        tracing::warn!("Ignoring unknown initializationOptions: {:?}", json);
    }

    let initialize_result = lsp_types::InitializeResult {
        capabilities: build_server_capabilities(&config),
        server_info: Some(lsp_types::ServerInfo {
            name: "nail-language-server".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
        offset_encoding: None,
    };
    let initialize_result = serde_json::to_value(initialize_result).unwrap();

    if let Err(e) = connection.initialize_finish(initialize_id, initialize_result) {
        if e.channel_is_disconnected() {
            io_threads.join()?;
        }
        return Err(e.into());
    }

    // main_loop(connection)?;

    io_threads.join()?;

    tracing::info!("server did shut down");

    Ok(())
}

fn from_json<T: DeserializeOwned>(
    what: &'static str,
    json: &serde_json::Value,
) -> anyhow::Result<T> {
    serde_json::from_value(json.clone())
        .map_err(|e| anyhow::format_err!("Failed to deserialize {what}: {e}; {json}"))
}

struct Config {
    root_path: std::path::PathBuf,
    client_capabilities: lsp_types::ClientCapabilities,
}
impl Config {
    fn new(
        root_path: std::path::PathBuf,
        client_capabilities: lsp_types::ClientCapabilities,
    ) -> Self {
        Self {
            root_path,
            client_capabilities,
        }
    }
}

fn build_server_capabilities(config: &Config) -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
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
    }
}
