//! Language Server Protocol implementation for Nail.

mod analysis;
mod context;
mod diagnostic;
mod language_server;
mod line_index;
mod semantic_tokens;

use language_server::NailLanguageServer;
use tower_lsp::{LspService, Server};

/// Run the language server.
pub async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(NailLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
