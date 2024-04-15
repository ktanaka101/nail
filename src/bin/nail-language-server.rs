use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, Registry};
use tracing_tree::HierarchicalLayer;

fn main() {
    setup_logging();

    // lsp::run_server().await
}

fn setup_logging() {
    Registry::default()
        .with(tracing_subscriber::fmt::layer().with_writer(std::io::stderr))
        .with(
            HierarchicalLayer::default()
                .with_indent_amount(2)
                .with_ansi(false)
                .with_writer(std::io::stderr),
        )
        .try_init()
        .expect("failed to initialize logging");
}
