mod context;
mod line_index;

use std::sync::Arc;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use context::Context;

pub async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(NailLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[derive(Debug, Clone)]
pub struct NailLanguageServer(Arc<tokio::sync::Mutex<Backend>>);

impl NailLanguageServer {
    pub fn new(client: Client) -> Self {
        Self(Arc::new(tokio::sync::Mutex::new(Backend {
            client,
            context: Context::default(),
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

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.0.lock().await.did_close(params).await
    }
}

#[derive(Debug)]
struct Backend {
    client: Client,
    context: Context,
}

impl Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult::default())
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, _params: DidOpenTextDocumentParams) {
        unimplemented!()
    }

    async fn did_close(&self, _params: DidCloseTextDocumentParams) {
        unimplemented!()
    }
}
