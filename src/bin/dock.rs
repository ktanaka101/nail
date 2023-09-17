//! Dock is Nail project's build system and package manager.

use std::ffi::{c_char, CString};

use anyhow::Result;
use clap::{Parser, Subcommand};
use codegen_llvm::CodegenContext;
use inkwell::{context::Context, OptimizationLevel};

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
            let Some(path)  = path else { panic!("The path must be specified.(Help: --path {{path}})"); };

            match execute(&path) {
                Ok(output) => {
                    println!("{}", output);
                }
                Err(e) => {
                    eprintln!("Error: {}", e);
                    std::process::exit(1);
                }
            }
        }
    }
}

fn execute(filepath: &str) -> Result<String> {
    let db = base_db::SalsaDatabase::default();
    let mut source_db = hir::SourceDatabase::new(&db, filepath.into());

    let pods = hir::parse_pods(&db, &mut source_db);
    let errors = pods.root_pod.root_hir_file.errors(&db);
    if !errors.is_empty() {
        return Err(anyhow::anyhow!("Hir error: {:?}", errors));
    }

    let ty_result = hir_ty::lower_pods(&db, &pods);
    let type_inference_errors = ty_result.type_inference_errors();
    if !type_inference_errors.is_empty() {
        return Err(anyhow::anyhow!("Ty error: {:?}", type_inference_errors));
    }
    let type_check_errors = ty_result.type_check_errors();
    if !type_check_errors.is_empty() {
        return Err(anyhow::anyhow!("Ty error: {:?}", type_check_errors));
    }

    let mir_result = mir::lower_pods(&db, &pods, &ty_result);

    let context = Context::create();
    let module = context.create_module("top");
    let builder = context.create_builder();
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();
    let codegen_result = codegen_llvm::codegen(
        &db,
        &mir_result,
        &CodegenContext {
            context: &context,
            module: &module,
            builder: &builder,
            execution_engine: &execution_engine,
        },
        true,
    );

    let result_string = {
        let c_string_ptr = unsafe { codegen_result.function.call() };
        unsafe { CString::from_raw(c_string_ptr as *mut c_char) }
            .into_string()
            .unwrap()
    };

    Ok(result_string)
}
