use std::{
    collections::HashMap,
    ffi::{c_char, CString},
    path,
};

use anyhow::Result;
use ariadne::{Label, Report, ReportKind, Source};
use codegen_llvm::CodegenContext;
use inkwell::{context::Context, OptimizationLevel};
use tokio::io;

/// ルートファイルのディレクトリ配下のNailファイルをすべて読み込み、ファイルパスと内容のマッピングを返す。
async fn read_nail_files(
    db: &dyn hir::HirMasterDatabase,
    root_nail_file_path: path::PathBuf,
) -> (hir::NailFile, HashMap<path::PathBuf, hir::NailFile>) {
    let nail_file_paths_exclude_root = collect_nail_files(root_nail_file_path.parent().unwrap());

    let mut read_file_futures = tokio::task::JoinSet::new();
    for nail_file_path in nail_file_paths_exclude_root {
        read_file_futures.spawn(read_nail_file(nail_file_path));
    }

    let mut file_by_path = HashMap::<path::PathBuf, hir::NailFile>::new();
    while let Some(contents) = read_file_futures.join_next().await {
        let (file_path, contents) = contents.expect("Failed to read file.");
        if let Ok(contents) = contents {
            let nail_file = hir::NailFile::new(db, file_path.clone(), contents, false);
            file_by_path.insert(file_path, nail_file);
        } else {
            panic!("Failed to read file.");
        }
    }

    let root_contents = std::fs::read_to_string(&root_nail_file_path).unwrap();
    let root_file = hir::NailFile::new(db, root_nail_file_path.clone(), root_contents, true);
    file_by_path.insert(root_nail_file_path, root_file);

    return (root_file, file_by_path);

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

    async fn read_nail_file(
        file_path: path::PathBuf,
    ) -> (path::PathBuf, Result<String, io::Error>) {
        let contents = tokio::fs::read_to_string(&file_path).await;
        (file_path, contents)
    }
}

pub async fn execute(root_nail_file_path: &str) -> Result<String> {
    let root_nail_file_path = path::PathBuf::try_from(root_nail_file_path).unwrap();

    let db = base_db::SalsaDatabase::default();

    let (root_file, file_by_path) = read_nail_files(&db, root_nail_file_path).await;
    let mut source_db = hir::SourceDatabase::new(root_file, file_by_path);

    let pods = hir::parse_pods(&db, &mut source_db);
    let errors = pods.root_pod.root_hir_file.errors(&db);
    if !errors.is_empty() {
        return Err(anyhow::anyhow!("Hir error: {:?}", errors));
    }

    let ty_result = hir_ty::lower_pods(&db, &pods);
    let type_inference_errors = ty_result.type_inference_errors();
    if !type_inference_errors.is_empty() {
        for error in &type_inference_errors {
            print_error(&db, &pods.root_pod.root_source_map, error);
        }

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

fn print_error(
    db: &base_db::SalsaDatabase,
    source_map: &hir::HirFileSourceMap,
    error: &hir_ty::InferenceError,
) {
    use hir_ty::InferenceError;
    let file = source_map.file(db);

    match error {
        InferenceError::MismatchedTypes {
            expected_ty,
            expected_expr,
            found_ty,
            found_expr,
        } => {
            let offset = source_map
                .source_by_expr(db)
                .get(found_expr)
                .unwrap()
                .clone()
                .value
                .node
                .text_range()
                .start();

            Report::build(
                ReportKind::Error,
                file.file_path(db).to_str().unwrap(),
                offset.into(),
            )
            .with_label(Label::new((file.file_path(db).to_str().unwrap(), 0..1)))
            .finish()
            .eprint((
                file.file_path(db).to_str().unwrap(),
                Source::from(file.contents(db)),
            ))
            .unwrap();
        }
        InferenceError::MismatchedTypeIfCondition {
            expected_condition_bool_ty,
            found_condition_ty,
            found_condition_expr,
        } => todo!(),
        InferenceError::MismatchedTypeElseBranch {
            then_branch_ty,
            then_branch,
            else_branch_ty,
            else_branch,
        } => todo!(),
        InferenceError::MismatchedTypeOnlyIfBranch {
            then_branch_ty,
            then_branch,
            else_branch_unit_ty,
        } => todo!(),
        InferenceError::MismaatchedTypeCallArg {
            expected_ty,
            found_ty,
            expected_signature,
            found_expr,
            arg_pos,
        } => todo!(),
        InferenceError::MismatchedBinaryInteger {
            expected_int_ty,
            found_expr,
            found_ty,
            op,
        } => todo!(),
        InferenceError::MismatchedBinaryCompare {
            compare_from_ty,
            compare_from_expr,
            compare_to_ty,
            compare_to_expr,
            op,
        } => todo!(),
        InferenceError::MismatchedUnary {
            expected_ty,
            found_ty,
            found_expr,
            op,
        } => todo!(),
        InferenceError::MismatchedTypeReturnExpr {
            expected_signature,
            found_ty,
            found_return_expr,
            found_return,
        } => {
            let text_range = source_map
                .source_by_expr(db)
                .get(found_return)
                .unwrap()
                .clone()
                .value
                .node
                .text_range();
            let return_type = expected_signature.return_type(db);
            let return_type = type_to_string(db, &return_type);
            let found_ty = type_to_string(db, found_ty);

            Report::build(
                ReportKind::Error,
                file.file_path(db).to_str().unwrap(),
                text_range.start().into(),
            )
            .with_label(
                Label::new((
                    file.file_path(db).to_str().unwrap(),
                    (text_range.start().into())..(text_range.end().into()),
                ))
                .with_message(format!("expected {return_type}, actual: {found_ty}")),
            )
            .finish()
            .eprint((
                file.file_path(db).to_str().unwrap(),
                Source::from(file.contents(db)),
            ))
            .unwrap();
        }
        InferenceError::MismatchedTypeReturnValue {
            expected_signature,
            found_ty,
            found_last_expr,
        } => todo!(),
        InferenceError::MismatchedCallArgCount {
            expected_callee_arg_count,
            found_arg_count,
        } => todo!(),
        InferenceError::NotCallable {
            found_callee_ty,
            found_callee_symbol,
            found_callee_expr,
        } => todo!(),
        InferenceError::ModuleAsExpr {
            found_module,
            found_expr,
        } => todo!(),
    };
}

fn type_to_string(db: &base_db::SalsaDatabase, ty: &hir_ty::Monotype) -> String {
    match ty {
        hir_ty::Monotype::Integer => "int".to_string(),
        hir_ty::Monotype::Bool => "bool".to_string(),
        hir_ty::Monotype::Unit => "()".to_string(),
        hir_ty::Monotype::Function(signature) => {
            format!(
                "{} -> {}",
                signature
                    .params(db)
                    .iter()
                    .map(|param| type_to_string(db, param))
                    .collect::<Vec<String>>()
                    .join(", "),
                type_to_string(db, &signature.return_type(db))
            )
        }
        hir_ty::Monotype::Char => "char".to_string(),
        hir_ty::Monotype::String => "string".to_string(),
        hir_ty::Monotype::Variable(_) => unreachable!(),
        hir_ty::Monotype::Never => "!".to_string(),
        hir_ty::Monotype::Unknown => "<unknown>".to_string(),
    }
}
