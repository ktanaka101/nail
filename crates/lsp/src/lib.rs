mod context;
mod line_index;
mod semantic_tokens;
mod server;

use server::NailLanguageServer;
use tower_lsp::{LspService, Server};

pub async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(NailLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
