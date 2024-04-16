//! Language Server Protocol implementation for Nail.

mod analysis;
// mod language_server;
mod line_index;
mod semantic_tokens;

use std::{collections::HashMap, env, path::PathBuf};

use analysis::Analysis;
use crossbeam_channel::{Receiver, Sender};
use hir::SourceDatabaseTrait;
use lsp_server::{Connection, Message, RequestId, Response};
use lsp_types::{
    notification::Notification, request::Request, DidChangeTextDocumentParams, DocumentFilter,
    PublishDiagnosticsParams, SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensParams, SemanticTokensRegistrationOptions,
    SemanticTokensServerCapabilities, ServerCapabilities, StaticRegistrationOptions,
    TextDocumentRegistrationOptions, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    WorkDoneProgressOptions,
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

    main_loop(config, connection)?;

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

fn build_server_capabilities(_config: &Config) -> ServerCapabilities {
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

fn main_loop(config: Config, connection: Connection) -> anyhow::Result<()> {
    GlobalState::new(config, connection.sender).run(connection.receiver)?;

    Ok(())
}

struct GlobalState {
    sender: Sender<lsp_server::Message>,
    config: Config,
    db: base_db::SalsaDatabase,
    analysis_by_uri: HashMap<Url, Analysis>,

    shutdown_requested: bool,
}

impl GlobalState {
    fn new(config: Config, sender: Sender<lsp_server::Message>) -> Self {
        Self {
            sender,
            config,
            db: base_db::SalsaDatabase::default(),
            shutdown_requested: false,
            analysis_by_uri: HashMap::new(),
        }
    }

    fn run(mut self, receiver: Receiver<lsp_server::Message>) -> anyhow::Result<()> {
        for msg in &receiver {
            match msg {
                lsp_server::Message::Request(req) => {
                    tracing::info!("handle request: {:?}", req.id);
                    self.handle_request(req)?;
                }
                lsp_server::Message::Notification(not) => {
                    tracing::info!("handle notification: {:?}", not.method);

                    if not.method.as_str() == lsp_types::notification::Exit::METHOD {
                        return Ok(());
                    }

                    self.handle_noification(not)?;
                }
                lsp_server::Message::Response(res) => {
                    tracing::info!("unsupport response: {:?}", res.id);
                }
            }
        }

        Ok(())
    }

    fn handle_request(&mut self, req: lsp_server::Request) -> anyhow::Result<()> {
        match req.method.as_str() {
            lsp_types::request::Shutdown::METHOD => {
                if self.shutdown_requested {
                    let resp = Response::new_err(
                        req.id.clone(),
                        lsp_server::ErrorCode::InvalidRequest as i32,
                        "Shutdown already requested.".to_owned(),
                    );
                    self.sender.send(resp.into())?;
                } else {
                    self.shutdown_requested = true;
                    let resp = Response::new_ok(req.id.clone(), ());
                    self.sender.send(resp.into())?;
                }
            }
            lsp_types::request::SemanticTokensFullRequest::METHOD => {
                let (req_id, params) =
                    extract_request::<lsp_types::request::SemanticTokensFullRequest>(req)?;
                self.semantic_tokens_full(req_id, params)?;
            }
            _ => {
                tracing::warn!("unhandled request: {:?}", req.method);
            }
        }

        Ok(())
    }

    fn handle_noification(&mut self, not: lsp_server::Notification) -> anyhow::Result<()> {
        match not.method.as_str() {
            lsp_types::notification::DidOpenTextDocument::METHOD => {
                let params =
                    extract_notification::<lsp_types::notification::DidOpenTextDocument>(not)?;
                self.did_open(params)?;
            }
            lsp_types::notification::DidChangeTextDocument::METHOD => {
                let params =
                    extract_notification::<lsp_types::notification::DidChangeTextDocument>(not)?;
                self.did_change(params)?;
            }
            _ => {
                tracing::warn!("unhandled notification: {:?}", not.method);
            }
        }

        Ok(())
    }

    /// Handle `textDocument/semanticTokens/full` request.
    fn semantic_tokens_full(
        &self,
        req_id: RequestId,
        params: SemanticTokensParams,
    ) -> anyhow::Result<()> {
        tracing::info!(
            "server semantic tokens full! get uri: {}",
            params.text_document.uri
        );

        if let Some(analysis) = self.analysis_by_uri.get(&params.text_document.uri) {
            let tokens = analysis.semantic_tokens();
            self.sender.send(Message::Response(Response::new_ok(
                req_id,
                SemanticTokens {
                    result_id: None,
                    data: tokens,
                },
            )))?;
        }

        Ok(())
    }

    /// Handle `textDocument/didOpen` notification.
    fn did_open(&mut self, params: lsp_types::DidOpenTextDocumentParams) -> anyhow::Result<()> {
        let opened_uri = params.text_document.uri;
        tracing::info!("server did open! get uri: {opened_uri}");

        let Ok(source_db) = build_source_db(&self.db, self.config.root_path.clone()) else {
            tracing::info!("failed to build source db");
            return Ok(());
        };
        let analysis = match get_analysis_by_file(&self.db, &source_db, opened_uri.clone()) {
            Ok(Some(analysis)) => analysis,
            Ok(None) => {
                tracing::info!("failed to get analysis by file: {opened_uri}");
                return Ok(());
            }
            Err(e) => {
                tracing::info!("failed to get analysis by file: {opened_uri}, error: {e}");
                return Ok(());
            }
        };

        let diagnostics = analysis.diagnostics.clone();
        self.analysis_by_uri.insert(opened_uri.clone(), analysis);

        self.publish_diagnostics(opened_uri, diagnostics, None)?;

        Ok(())
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> anyhow::Result<()> {
        let changed_file_uri = params.text_document.uri;
        tracing::info!("server did change! get uri: {}", changed_file_uri);

        let Ok(source_db) = build_source_db(&self.db, self.config.root_path.clone()) else {
            tracing::info!("failed to build source db");
            return Ok(());
        };

        let Ok(changed_file_path) = changed_file_uri.to_file_path() else {
            tracing::info!(
                "failed to convert file url to file path. file_url: {changed_file_uri:?}"
            );
            return Ok(());
        };

        let Some(changed_file) = source_db.get_file(&changed_file_path) else {
            tracing::info!("file not found in source db. file_path: {changed_file_path:?}");
            return Ok(());
        };

        let mut contents = changed_file.contents(&self.db).clone();
        let mut line_index = line_index::LineIndex::new(&contents);
        params.content_changes.into_iter().for_each(|change| {
            if let Some(range) = change.range {
                let range: line_index::PositionRange = range.into();
                let range = line_index.range(range);
                contents.replace_range(range, &change.text);
            } else {
                contents = change.text;
                // todo: 一度に全ての行を再計算するのは効率が悪いので、変更箇所のみを再計算するようにする
                line_index = line_index::LineIndex::new(&contents);
            }
        });
        changed_file.set_contents(&mut self.db).to(contents);

        let analysis = match get_analysis_by_file(&self.db, &source_db, changed_file_uri.clone()) {
            Ok(Some(analysis)) => analysis,
            Ok(None) => {
                tracing::info!("failed to get analysis by file: {changed_file_uri}");
                return Ok(());
            }
            Err(e) => {
                tracing::info!("failed to get analysis by file: {changed_file_uri}, error: {e}");
                return Ok(());
            }
        };
        let diagnostics = analysis.diagnostics.clone();
        self.analysis_by_uri
            .insert(changed_file_uri.clone(), analysis);

        self.publish_diagnostics(changed_file_uri, diagnostics, None)?;

        Ok(())
    }

    /// Publish diagnostics to client.
    fn publish_diagnostics(
        &self,
        uri: Url,
        diagnostics: Vec<lsp_types::Diagnostic>,
        version: Option<i32>,
    ) -> anyhow::Result<()> {
        self.sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp_types::notification::PublishDiagnostics::METHOD.to_owned(),
                PublishDiagnosticsParams {
                    uri,
                    diagnostics,
                    version,
                },
            )))?;

        Ok(())
    }
}

fn extract_notification<N>(not: lsp_server::Notification) -> anyhow::Result<N::Params>
where
    N: lsp_types::notification::Notification,
    N::Params: DeserializeOwned + Send + std::fmt::Debug,
{
    Ok(not.extract::<N::Params>(N::METHOD)?)
}

fn extract_request<R>(req: lsp_server::Request) -> anyhow::Result<(RequestId, R::Params)>
where
    R: lsp_types::request::Request,
    R::Params: DeserializeOwned + Send + std::fmt::Debug,
{
    Ok(req.extract::<R::Params>(R::METHOD)?)
}

/// ソースコードを管理するデータベースを構築します。
fn build_source_db(
    db: &base_db::SalsaDatabase,
    root_dir_path: PathBuf,
) -> anyhow::Result<hir::SourceDatabase> {
    let root_pod_file_path = root_dir_path.join("pod.toml");
    let (root_nail_file, nail_file_by_path) =
        dock::read_nail_files_with_pod_file(db, root_pod_file_path)?;

    Ok(hir::SourceDatabase::new(root_nail_file, nail_file_by_path))
}

/// 指定したファイルの診断結果を返します。
fn get_analysis_by_file(
    db: &base_db::SalsaDatabase,
    source_db: &hir::SourceDatabase,
    file_url: Url,
) -> anyhow::Result<Option<Analysis>> {
    let Ok(file_path) = file_url.to_file_path() else {
        anyhow::bail!("failed to convert file url to file path. file_url: {file_url:?}");
    };

    let Some(target_nail_file) = source_db.get_file(&file_path) else {
        anyhow::bail!("file not found in source db. file_path: {file_path:?}");
    };
    let mut diagnostics: Vec<diagnostic::Diagnostic> = vec![];

    let pods = hir::parse_pods(db, source_db);
    // todo: display parser and validation errors
    // todo: display lower error of all files
    let target_hir_file = if target_nail_file.root(db) {
        &pods.root_pod.root_hir_file
    } else {
        let Some(hir_file) = pods.root_pod.get_hir_file_by_file(target_nail_file) else {
            anyhow::bail!("file_path is not module contains. file_path: {file_path:?}");
        };
        hir_file
    };

    let errors = target_hir_file.errors(db);
    diagnostics.append(
        errors
            .iter()
            .map(|error| {
                diagnostic::Diagnostic::from_hir_lower_error(
                    db,
                    // todo: 複数のPodに対応
                    &pods.root_pod.root_source_map,
                    error,
                )
            })
            .collect::<Vec<_>>()
            .as_mut(),
    );

    let ty_result = hir_ty::lower_pods(db, &pods);
    let type_inference_errors = ty_result.type_inference_errors_with_function();
    if !type_inference_errors.is_empty() {
        for (function, error) in &type_inference_errors {
            let Some(hir_file) = pods.root_pod.get_hir_file_by_function(*function) else {
                anyhow::bail!("hir_file not found. function: {function:?}");
            };
            if hir_file.file(db) != target_nail_file {
                continue;
            }

            let Some(source_map) = pods.root_pod.source_map_by_function(db, *function) else {
                anyhow::bail!("source_map not found. function: {function:?}");
            };

            diagnostics.push(diagnostic::Diagnostic::from_hir_ty_inference_error(
                db,
                *hir_file,
                hir_file.db(db),
                source_map,
                error,
            ));
        }
    }

    // todo: display type check errors

    // todo: HIR構築と重複した処理なので削除したい
    let parsed = parser::parse(target_nail_file.contents(db));
    let line_index = line_index::LineIndex::new(target_nail_file.contents(db));
    let syntax = parsed.syntax();

    diagnostics.append(
        &mut parsed
            .errors()
            .iter()
            .map(|e| diagnostic::Diagnostic::from_parse_error(target_nail_file, e))
            .collect::<Vec<_>>(),
    );

    diagnostics.append(
        &mut ast::validation::validate(&syntax)
            .iter()
            .map(|e| diagnostic::Diagnostic::from_validation_error(target_nail_file, e))
            .collect::<Vec<_>>(),
    );

    let diagnostics = diagnostics
        .iter()
        .flat_map(|diagnostic| {
            let contents = diagnostic.file.contents(db);
            let line_index = line_index::LineIndex::new(contents);

            diagnostic.messages.iter().map(move |message| {
                let range = line_index::PositionRange::from_text_range(message.range, &line_index);
                lsp_types::Diagnostic::new_simple(range.into(), message.message.clone())
            })
        })
        .collect::<Vec<_>>();

    Ok(Some(Analysis {
        uri: file_url,
        file: target_nail_file,
        parsed,
        diagnostics,
        line_index,
    }))
}
