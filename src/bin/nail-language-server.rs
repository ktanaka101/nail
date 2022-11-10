#[tokio::main]
async fn main() {
    new_lsp::run_server().await
}
