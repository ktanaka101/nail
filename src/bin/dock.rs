//! Dock is Nail project's build system and package manager.

use std::io;

use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Nail実行コマンド
    Run {
        /// 実行対象のファイルパス
        #[arg(long)]
        path: Option<String>,
    },
}

fn main() {
    let args = Cli::parse();
    match args.command {
        Command::Run { path } => {
            let Some(path) = path else {
                panic!("The path must be specified.(Help: --path {{path}})");
            };
            let out = &mut io::stdout();
            let err = &mut io::stderr();

            match dock::execute(
                dock::NailExecutablePath::RootFile(path.into()),
                out,
                err,
                true,
            ) {
                Ok(_) => (),
                Err(err) => {
                    match err {
                        dock::ExecutionError::Io(e) => {
                            eprintln!("IO error: {}", e);
                        }
                        dock::ExecutionError::Nail => (),
                        dock::ExecutionError::InvalidRootNailFilePath(e) => {
                            eprintln!("Invalid root nail file path: {}", e);
                        }
                    }
                    std::process::exit(1);
                }
            }
        }
    }
}
