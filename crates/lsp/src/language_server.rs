use std::{collections::HashMap, path::PathBuf, sync::Arc};

use base_db::SalsaDatabase;
use hir::SourceDatabaseTrait;
use tokio::sync::Mutex;
use tower_lsp::{
    jsonrpc,
    lsp_types::{self, *},
    Client, LanguageServer,
};

use crate::{analysis::Analysis, line_index, semantic_tokens::SEMANTIC_TOKEN_TYPES};

pub struct NailLanguageServer(Arc<Mutex<Backend>>);

impl NailLanguageServer {
    pub fn new(client: Client) -> Self {
        Self(Arc::new(Mutex::new(Backend {
            client,
            db: base_db::SalsaDatabase::default(),
            root_dir_path: None,
            analysis_by_uri: HashMap::new(),
        })))
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for NailLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        self.0.lock().await.initialize(params).await
    }

    async fn initialized(&self, params: InitializedParams) {
        self.0.lock().await.initialized(params).await
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
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
    ) -> jsonrpc::Result<Option<SemanticTokensResult>> {
        self.0.lock().await.semantic_tokens_full(params).await
    }
}

struct Backend {
    client: Client,
    db: SalsaDatabase,
    root_dir_path: Option<PathBuf>,
    analysis_by_uri: HashMap<Url, Analysis>,
}

impl Backend {
    async fn initialize(&mut self, params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        self.info("server initialize!").await;

        let root_dir_path = if let Some(root_uri) = &params.root_uri {
            if let Ok(root_dir_path) = root_uri.to_file_path() {
                root_dir_path
            } else {
                return Err(jsonrpc::Error::invalid_params(
                    "root_uri must be a file path.",
                ));
            }
        } else {
            return Err(jsonrpc::Error::invalid_params("root_uri is required"));
        };
        self.root_dir_path = Some(root_dir_path);

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

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        self.info("server shutdown!").await;
        Ok(())
    }

    async fn did_open(&mut self, params: DidOpenTextDocumentParams) {
        let opened_uri = params.text_document.uri;
        self.info(&format!("server did open! get uri: {opened_uri}"))
            .await;

        let Some(root_dir_path) = &self.root_dir_path else {
            self.info("root_dir_path is not set.").await;
            return;
        };

        let Ok(source_db) = build_source_db(&self.db, root_dir_path.clone()).await else {
            self.info("failed to build source db").await;
            return;
        };
        let analysis = match get_analysis_by_file(&self.db, &source_db, opened_uri.clone()).await {
            Ok(Some(analysis)) => analysis,
            Ok(None) => {
                self.info(&format!("failed to get analysis by file: {opened_uri}"))
                    .await;
                return;
            }
            Err(e) => {
                self.info(&format!(
                    "failed to get analysis by file: {opened_uri}, error: {e}",
                ))
                .await;
                return;
            }
        };

        let diagnostics = analysis.diagnostics.clone();
        self.analysis_by_uri.insert(opened_uri.clone(), analysis);

        self.client
            .publish_diagnostics(opened_uri, diagnostics, None)
            .await;
    }

    async fn did_change(&mut self, params: DidChangeTextDocumentParams) {
        let changed_file_uri = params.text_document.uri;
        self.info(&format!("server did change! get uri: {}", changed_file_uri))
            .await;

        let Some(root_dir_path) = &self.root_dir_path else {
            self.info("root_dir_path is not set.").await;
            return;
        };

        let Ok(source_db) = build_source_db(&self.db, root_dir_path.clone()).await else {
            self.info("failed to build source db").await;
            return;
        };

        let Ok(changed_file_path) = changed_file_uri.to_file_path() else {
            self.info(&format!(
                "failed to convert file url to file path. file_url: {changed_file_uri:?}"
            ))
            .await;
            return;
        };

        let Some(changed_file) = source_db.get_file(&changed_file_path) else {
            self.info(&format!(
                "file not found in source db. file_path: {changed_file_path:?}"
            ))
            .await;
            return;
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

        let analysis =
            match get_analysis_by_file(&self.db, &source_db, changed_file_uri.clone()).await {
                Ok(Some(analysis)) => analysis,
                Ok(None) => {
                    self.info(&format!(
                        "failed to get analysis by file: {changed_file_uri}"
                    ))
                    .await;
                    return;
                }
                Err(e) => {
                    self.info(&format!(
                        "failed to get analysis by file: {changed_file_uri}, error: {e}",
                    ))
                    .await;
                    return;
                }
            };
        let diagnostics = analysis.diagnostics.clone();
        self.analysis_by_uri
            .insert(changed_file_uri.clone(), analysis);

        self.client
            .publish_diagnostics(changed_file_uri, diagnostics, None)
            .await;
    }

    async fn did_close(&mut self, params: DidCloseTextDocumentParams) {
        self.info(&format!(
            "server did close! get uri: {}",
            params.text_document.uri
        ))
        .await;

        // todo
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> jsonrpc::Result<Option<SemanticTokensResult>> {
        self.info(&format!(
            "server semantic tokens full! get uri: {}",
            params.text_document.uri
        ))
        .await;

        if let Some(analysis) = self.analysis_by_uri.get(&params.text_document.uri) {
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

/// ソースコードを管理するデータベースを構築します。
async fn build_source_db(
    db: &SalsaDatabase,
    root_dir_path: PathBuf,
) -> anyhow::Result<hir::SourceDatabase> {
    let root_pod_file_path = root_dir_path.join("pod.toml");
    let (root_nail_file, nail_file_by_path) =
        dock::read_nail_files_with_pod_file(db, root_pod_file_path).await?;

    Ok(hir::SourceDatabase::new(root_nail_file, nail_file_by_path))
}

/// 指定したファイルの診断結果を返します。
async fn get_analysis_by_file(
    db: &SalsaDatabase,
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
