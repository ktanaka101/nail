use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    action: Action,
}

#[derive(Debug, Subcommand)]
enum Action {
    Run,
}

fn main() {
    let args = Args::parse();
    match args.action {
        Action::Run => {
            todo!()
        }
    }
}
