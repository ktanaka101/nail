//! DockはNailプログラムのプロジェクト管理/パッケージ管理/ビルドシステムの機能を持ちます。

use std::{
    collections::HashMap,
    ffi::{c_char, CString},
    io,
    path::{self, PathBuf},
};

use anyhow::Result;
use ariadne::{Label, Report, ReportKind, Source};
use codegen_llvm::CodegenContext;
use inkwell::{context::Context, OptimizationLevel};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use thiserror::Error;

/// Podファイルを基準に、ディレクトリ配下のNailファイルをすべて読み込み、ファイルパスと内容のマッピングを返す。
pub fn read_nail_files_with_pod_file(
    db: &dyn hir::HirMasterDatabase,
    root_nail_file_path: path::PathBuf,
) -> Result<(hir::NailFile, HashMap<path::PathBuf, hir::NailFile>), ExecutionError> {
    let root_nail_file_path = root_nail_file_path.parent().unwrap().join("src/main.nail");
    read_nail_files_with_root_nail_file(db, root_nail_file_path)
}

/// ルートファイルを基準に、ディレクトリ配下のNailファイルをすべて読み込み、ファイルパスと内容のマッピングを返す。
pub fn read_nail_files_with_root_nail_file(
    db: &dyn hir::HirMasterDatabase,
    root_nail_file_path: path::PathBuf,
) -> Result<(hir::NailFile, HashMap<path::PathBuf, hir::NailFile>), ExecutionError> {
    let nail_file_paths_exclude_root = collect_nail_files(root_nail_file_path.parent().unwrap());

    let file_contents: std::io::Result<Vec<(PathBuf, String)>> = nail_file_paths_exclude_root
        .into_par_iter()
        .map(|file_path| std::fs::read_to_string(&file_path).map(|contents| (file_path, contents)))
        .collect();

    let mut file_by_path = HashMap::<path::PathBuf, hir::NailFile>::new();
    match file_contents {
        Ok(contents) => {
            for contents in contents {
                let (file_path, contents) = contents;
                let nail_file = hir::NailFile::new(db, file_path.clone(), contents, false);
                file_by_path.insert(file_path, nail_file);
            }
        }
        Err(e) => return Err(ExecutionError::Io(e)),
    }

    let root_contents = std::fs::read_to_string(&root_nail_file_path).unwrap();
    let root_file = hir::NailFile::new(db, root_nail_file_path.clone(), root_contents, true);
    file_by_path.insert(root_nail_file_path, root_file);

    return Ok((root_file, file_by_path));

    fn collect_nail_files(dir: &path::Path) -> Vec<path::PathBuf> {
        assert!(
            dir.is_dir(),
            "assertion failed: dir.is_dir(), dir: {:?}",
            dir
        );

        let mut nail_files = vec![];

        let read_dir = std::fs::read_dir(dir).expect("Failed to read root directory.");
        for child_dir_entry in read_dir {
            if let Ok(child_entry) = child_dir_entry {
                let child_path = child_entry.path();
                if child_path.is_file() && child_path.extension().unwrap() == "nail" {
                    nail_files.push(child_path);
                } else if child_path.is_dir() {
                    nail_files.append(&mut collect_nail_files(&child_path));
                }
            } else {
                panic!("Failed to read file in root directory.");
            }
        }

        nail_files
    }
}

/// Nailプログラムの実行時に発生するエラー
#[derive(Error, Debug)]
pub enum ExecutionError {
    /// Nailプログラムのコンパイルエラー
    #[error("Compile nail error")]
    Nail,

    /// ルートNailファイルのパスが不正
    #[error("Invalid root nail file path: `{0}`")]
    InvalidRootNailFilePath(String),

    /// IOエラー
    #[error("IO error")]
    Io(#[from] io::Error),
}

/// 指定ファイルの種類
pub enum NailExecutablePath {
    /// Podファイルで実行する場合
    ///
    /// `pod.toml`を指定します。
    PodFile(path::PathBuf),

    /// nailファイルのみで実行する場合
    ///
    /// `main.nail`を指定します。
    /// 基本的には[NailExecutablePath::PodFile]を使用します。
    /// テストコードなど、簡易にnailファイルのみで実行したい場合に使用します。
    RootFile(path::PathBuf),
}
impl NailExecutablePath {
    /// ファイルパスを取得します。
    pub fn as_path(&self) -> &path::Path {
        match self {
            NailExecutablePath::PodFile(pod_file_path) => pod_file_path,
            NailExecutablePath::RootFile(root_nail_file_path) => root_nail_file_path,
        }
    }
}

/// Nailプログラムを実行します。
///
/// 通常の結果は`write_dest_out`、エラーは`write_dest_err`に書き込まれます。
pub fn execute(
    nail_executable_path: NailExecutablePath,
    write_dest_out: &mut impl std::io::Write,
    write_dest_err: &mut impl std::io::Write,
    enabled_color: bool,
) -> Result<(), ExecutionError> {
    let db = base_db::SalsaDatabase::default();

    let (root_file, file_by_path) = match nail_executable_path {
        NailExecutablePath::PodFile(pod_file_path) => {
            read_nail_files_with_pod_file(&db, pod_file_path)?
        }
        NailExecutablePath::RootFile(nail_file_path) => {
            read_nail_files_with_root_nail_file(&db, nail_file_path)?
        }
    };

    let source_db = hir::SourceDatabase::new(root_file, file_by_path);

    let pods = hir::parse_pods(&db, &source_db);
    let errors = pods.root_pod.root_hir_file.errors(&db);
    if !errors.is_empty() {
        write_dest_err.write_all(format!("Hir error: {:?}", errors).as_bytes())?;
        return Err(ExecutionError::Nail);
    }

    let ty_result = hir_ty::lower_pods(&db, &pods);
    let type_inference_errors = ty_result.type_inference_errors_with_function();
    let config = ariadne::Config::default().with_color(enabled_color);
    if !type_inference_errors.is_empty() {
        for (function, error) in &type_inference_errors {
            let hir_file = pods.root_pod.get_hir_file_by_function(*function).unwrap();
            let source_map = pods
                .root_pod
                .source_map_by_function(&db, *function)
                .unwrap();
            let file_db = hir_file.db(&db);
            let diagnostic = diagnostic::Diagnostic::from_hir_ty_inference_error(
                &db, *hir_file, file_db, source_map, error,
            );

            print_error(&db, diagnostic, write_dest_err, config);
        }
        return Err(ExecutionError::Nail);
    }

    let type_check_errors = ty_result.type_check_errors_with_function();
    if !type_check_errors.is_empty() {
        for (function, error) in &type_check_errors {
            let hir_file = pods.root_pod.get_hir_file_by_function(*function).unwrap();
            let source_map = pods
                .root_pod
                .source_map_by_function(&db, *function)
                .unwrap();
            let file_db = hir_file.db(&db);
            let diagnostic = diagnostic::Diagnostic::from_hir_ty_check_error(
                &db, *hir_file, file_db, source_map, error,
            );

            print_error(&db, diagnostic, write_dest_err, config);
        }
        return Err(ExecutionError::Nail);
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
    write_dest_out.write_all(result_string.as_bytes())?;

    Ok(())
}

fn print_error(
    db: &base_db::SalsaDatabase,
    diagnostic: diagnostic::Diagnostic,
    write_dest_err: &mut impl std::io::Write,
    config: ariadne::Config,
) {
    let file = diagnostic.file;
    let file_path = file.file_path(db).to_str().unwrap().to_string();
    let labels = diagnostic.messages.into_iter().map(|message| {
        Label::new((&file_path, message.range.into())).with_message(message.message)
    });

    Report::build(ReportKind::Error, &file_path, diagnostic.head_offset)
        .with_message(diagnostic.title)
        .with_labels(labels)
        .with_config(config)
        .finish()
        .write(
            (&file_path, Source::from(diagnostic.file.contents(db))),
            write_dest_err,
        )
        .unwrap();
}
