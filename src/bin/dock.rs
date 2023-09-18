//! Dock is Nail project's build system and package manager.

use std::{
    collections::HashMap,
    ffi::{c_char, CString},
    path,
};

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

fn collect_nail_files(dir: &path::Path) -> Vec<path::PathBuf> {
    assert!(dir.is_dir());

    let mut nail_files = vec![];

    let read_dir = std::fs::read_dir(dir).expect("Failed to read root directory.");
    for child_dir_entry in read_dir {
        if let Ok(child_entry) = child_dir_entry {
            let child_path = child_entry.path();
            if child_path.is_file() && child_path.extension().unwrap() == "nail" {
                nail_files.push(child_path);
            } else {
                nail_files.append(&mut collect_nail_files(&child_path));
            }
        } else {
            panic!("Failed to read file in root directory.");
        }
    }

    nail_files
}

fn execute(root_nail_file_path: &str) -> Result<String> {
    let root_nail_file_path = path::PathBuf::try_from(root_nail_file_path).unwrap();

    let db = base_db::SalsaDatabase::default();

    let root_contents = std::fs::read_to_string(&root_nail_file_path).unwrap();
    let root_file = hir::NailFile::new(&db, root_nail_file_path.clone(), root_contents, true);

    let mut nail_file_paths = collect_nail_files(root_nail_file_path.parent().unwrap());
    nail_file_paths.push(root_nail_file_path);

    let mut file_by_path = HashMap::<path::PathBuf, hir::NailFile>::new();
    for nail_file_path in nail_file_paths {
        let contents = std::fs::read_to_string(&nail_file_path).unwrap();
        let file = hir::NailFile::new(&db, nail_file_path.clone(), contents, false);
        file_by_path.insert(nail_file_path, file);
    }

    let mut source_db = hir::SourceDatabase::new(root_file, file_by_path);

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
