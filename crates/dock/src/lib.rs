use std::{
    collections::HashMap,
    ffi::{c_char, CString},
    path,
};

use anyhow::Result;
use ariadne::{Label, Report, ReportKind, Source};
use ast::AstNode;
use codegen_llvm::CodegenContext;
use inkwell::{context::Context, OptimizationLevel};
use thiserror::Error;
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

    async fn read_nail_file(
        file_path: path::PathBuf,
    ) -> (path::PathBuf, Result<String, io::Error>) {
        let contents = tokio::fs::read_to_string(&file_path).await;
        (file_path, contents)
    }
}

#[derive(Error, Debug)]
pub enum ExecutionError {
    #[error("Compile nail error")]
    Nail,

    #[error("IO error")]
    Io(#[from] io::Error),
}

/// Nailプログラムを実行します。
///
/// 通常の結果は`write_dest_out`、エラーは`write_dest_err`に書き込まれます。
pub async fn execute(
    root_nail_file_path: &str,
    write_dest_out: &mut impl std::io::Write,
    write_dest_err: &mut impl std::io::Write,
    enabled_color: bool,
) -> Result<(), ExecutionError> {
    let root_nail_file_path = path::PathBuf::try_from(root_nail_file_path).unwrap();

    let db = base_db::SalsaDatabase::default();

    let (root_file, file_by_path) = read_nail_files(&db, root_nail_file_path).await;
    let mut source_db = hir::SourceDatabase::new(root_file, file_by_path);

    let pods = hir::parse_pods(&db, &mut source_db);
    let errors = pods.root_pod.root_hir_file.errors(&db);
    if !errors.is_empty() {
        write_dest_err.write_all(format!("Hir error: {:?}", errors).as_bytes())?;
        return Err(ExecutionError::Nail);
    }

    let ty_result = hir_ty::lower_pods(&db, &pods);
    let type_inference_errors = ty_result.type_inference_errors();
    let config = ariadne::Config::default().with_color(enabled_color);
    if !type_inference_errors.is_empty() {
        for error in &type_inference_errors {
            print_error(
                &db,
                pods.root_pod.root_hir_file.db(&db),
                &pods.root_pod.root_source_map,
                error,
                write_dest_err,
                config,
            );
        }
        return Err(ExecutionError::Nail);
    }
    let type_check_errors = ty_result.type_check_errors();
    if !type_check_errors.is_empty() {
        write_dest_err.write_all(format!("Ty error: {:?}", type_check_errors).as_bytes())?;
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

#[derive(Debug)]
struct Diagnostic {
    file: hir::NailFile,
    title: String,
    head_offset: usize,
    messages: Vec<Message>,
}
impl Diagnostic {
    fn from_error(
        db: &base_db::SalsaDatabase,
        file_db: &hir::HirFileDatabase,
        source_map: &hir::HirFileSourceMap,
        error: &hir_ty::InferenceError,
    ) -> Self {
        use hir_ty::InferenceError;
        let file = source_map.file(db);

        match error {
            InferenceError::MismatchedTypeIfCondition {
                expected_condition_bool_ty,
                found_condition_ty,
                found_condition_expr,
            } => {
                let text_range = found_condition_expr.text_range(db, source_map);
                let cond_type = type_to_string(db, expected_condition_bool_ty);
                let found_cond_ty = type_to_string(db, found_condition_ty);

                Diagnostic {
                    file,
                    title: "Mismatched type in if condition".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected {cond_type}, actual: {found_cond_ty}"),
                        range: (text_range.start().into())..(text_range.end().into()),
                    }],
                }
            }
            InferenceError::MismatchedTypeElseBranch {
                then_branch_ty,
                then_branch,
                else_branch_ty,
                else_branch,
            } => {
                let then_branch_range = then_branch.text_range(db, source_map);
                let else_branch_range = else_branch.text_range(db, source_map);
                let hir::Expr::Block(then_block) =  then_branch.lookup(file_db) else { unreachable!() };
                let then_tail_range: std::ops::Range<usize> = if let Some(tail) = then_block.tail {
                    tail.text_range(db, source_map).into()
                } else {
                    then_branch_range.end().into()..then_branch_range.end().into()
                };
                let hir::Expr::Block(else_block) =  else_branch.lookup(file_db) else { unreachable!() };
                let else_tail_range: std::ops::Range<usize> = if let Some(tail) = else_block.tail {
                    tail.text_range(db, source_map).into()
                } else {
                    else_branch_range.end().into()..else_branch_range.end().into()
                };

                let then_branch_ty = type_to_string(db, then_branch_ty);
                let else_branch_ty = type_to_string(db, else_branch_ty);

                Diagnostic {
                    file,
                    title: "Mismatched type if branch and else branch".to_string(),
                    head_offset: then_branch_range.start().into(),
                    messages: vec![
                        Message {
                            message: format!("expected {then_branch_ty}, actual: {else_branch_ty}"),
                            range: (then_branch_range.start().into())
                                ..(else_branch_range.start().into()),
                        },
                        Message {
                            range: then_tail_range,
                            message: format!("Type is {then_branch_ty}"),
                        },
                        Message {
                            range: else_tail_range,
                            message: format!("Type is {else_branch_ty}"),
                        },
                    ],
                }
            }
            InferenceError::MismatchedTypeOnlyIfBranch {
                then_branch_ty,
                then_branch,
                else_branch_unit_ty,
            } => {
                let then_branch_range = then_branch.text_range(db, source_map);
                let hir::Expr::Block(then_block) =  then_branch.lookup(file_db) else { unreachable!() };
                let then_tail_range: std::ops::Range<usize> = if let Some(tail) = then_block.tail {
                    tail.text_range(db, source_map).into()
                } else {
                    unreachable!("末尾の式がない場合は必ずUnitなのでエラーとならないはずです。");
                };

                let then_branch_ty = type_to_string(db, then_branch_ty);
                let else_branch_ty = type_to_string(db, else_branch_unit_ty);

                Diagnostic {
                    file,
                    title: "Mismatched type if branch and else branch".to_string(),
                    head_offset: then_branch_range.start().into(),
                    messages: vec![
                        Message {
                            message: format!("expected {else_branch_ty}, actual {then_branch_ty}"),
                            range: (then_branch_range.start().into())
                                ..(then_branch_range.end().into()),
                        },
                        Message {
                            range: then_tail_range,
                            message: format!("Type is {then_branch_ty}"),
                        },
                    ],
                }
            }
            InferenceError::MismaatchedTypeCallArg {
                expected_ty,
                found_ty,
                expected_signature: _,
                found_expr,
                arg_pos: _,
            } => {
                let text_range = found_expr.text_range(db, source_map);
                let expected_ty = type_to_string(db, expected_ty);
                let found_ty = type_to_string(db, found_ty);

                Diagnostic {
                    file,
                    title: "Mismatched type in call argument".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected {expected_ty}, actual: {found_ty}"),
                        range: (text_range.start().into())..(text_range.end().into()),
                    }],
                }
            }
            InferenceError::MismatchedBinaryInteger {
                expected_int_ty,
                found_expr,
                found_ty,
                op,
            } => {
                let text_range = found_expr.text_range(db, source_map);
                let expected_ty = type_to_string(db, expected_int_ty);
                let found_ty = type_to_string(db, found_ty);

                let op = match op {
                    ast::BinaryOp::Add(_) => "+",
                    ast::BinaryOp::Sub(_) => "-",
                    ast::BinaryOp::Mul(_) => "*",
                    ast::BinaryOp::Div(_) => "/",
                    ast::BinaryOp::Equal(_)
                    | ast::BinaryOp::GreaterThan(_)
                    | ast::BinaryOp::LessThan(_) => unreachable!(),
                };

                Diagnostic {
                    file,
                    title: format!("Mismatched type in binary integer expression `{op}`"),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected {expected_ty}, actual: {found_ty}"),
                        range: (text_range.start().into())..(text_range.end().into()),
                    }],
                }
            }
            InferenceError::MismatchedBinaryCompare {
                compare_from_ty,
                compare_from_expr,
                compare_to_ty,
                compare_to_expr,
                op,
            } => {
                let compare_from_range = compare_from_expr.text_range(db, source_map);
                let compare_to_range = compare_to_expr.text_range(db, source_map);
                let compare_from_ty = type_to_string(db, compare_from_ty);
                let compare_to_ty = type_to_string(db, compare_to_ty);

                let op = match op {
                    ast::BinaryOp::Equal(_) => "==",
                    ast::BinaryOp::GreaterThan(_) => ">",
                    ast::BinaryOp::LessThan(_) => "<",
                    ast::BinaryOp::Add(_)
                    | ast::BinaryOp::Sub(_)
                    | ast::BinaryOp::Mul(_)
                    | ast::BinaryOp::Div(_) => unreachable!(),
                };

                Diagnostic {
                    file,
                    title: format!("Mismatched type in binary compare expression `{op}`"),
                    head_offset: compare_from_range.start().into(),
                    messages: vec![
                        Message {
                            range: compare_from_range.into(),
                            message: format!("Type is {compare_from_ty}"),
                        },
                        Message {
                            range: compare_to_range.into(),
                            message: format!("Type is {compare_to_ty}"),
                        },
                    ],
                }
            }
            InferenceError::MismatchedUnary {
                expected_ty,
                found_ty,
                found_expr,
                op,
            } => {
                let text_range = found_expr.text_range(db, source_map);
                let expected_ty = type_to_string(db, expected_ty);
                let found_ty = type_to_string(db, found_ty);

                let op = match op {
                    ast::UnaryOp::Not(_) => "!",
                    ast::UnaryOp::Neg(_) => "-",
                };

                Diagnostic {
                    file,
                    title: format!("Mismatched type in unary expression `{op}`"),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected {expected_ty}, actual: {found_ty}"),
                        range: (text_range.start().into())..(text_range.end().into()),
                    }],
                }
            }
            InferenceError::MismatchedTypeReturnExpr {
                expected_signature,
                found_ty,
                found_return_expr: _,
                found_return,
            } => {
                let text_range = found_return.text_range(db, source_map);
                let return_type = expected_signature.return_type(db);
                let return_type = type_to_string(db, &return_type);
                let found_ty = type_to_string(db, found_ty);

                Diagnostic {
                    file,
                    title: "Mismatched type in return expression".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected {return_type}, actual: {found_ty}"),
                        range: (text_range.start().into())..(text_range.end().into()),
                    }],
                }
            }
            InferenceError::MismatchedTypeReturnValue {
                expected_signature,
                expected_function,
                found_ty,
                found_last_expr,
            } => {
                if let Some(found_last_expr) = found_last_expr {
                    let text_range = found_last_expr.text_range(db, source_map);
                    let return_type = expected_signature.return_type(db);
                    let return_type = type_to_string(db, &return_type);
                    let found_ty = type_to_string(db, found_ty);

                    Diagnostic {
                        file,
                        title: "Mismatched type in return value".to_string(),
                        head_offset: text_range.start().into(),
                        messages: vec![Message {
                            message: format!("expected {return_type}, actual: {found_ty}"),
                            range: (text_range.start().into())..(text_range.end().into()),
                        }],
                    }
                } else {
                    let text_range = {
                        source_map
                            .source_by_function(db)
                            .get(expected_function)
                            .unwrap()
                            .value
                            .return_type()
                            .unwrap()
                            .syntax()
                            .text_range()
                    };

                    let return_type = expected_signature.return_type(db);
                    let return_type = type_to_string(db, &return_type);
                    let found_ty = type_to_string(db, found_ty);

                    Diagnostic {
                        file,
                        title: "Mismatched type in return value".to_string(),
                        head_offset: text_range.start().into(),
                        messages: vec![Message {
                            message: format!(
                                "expected {return_type}, actual: {found_ty} as its body has tail"
                            ),
                            range: (text_range.start().into())..(text_range.end().into()),
                        }],
                    }
                }
            }
            InferenceError::MismatchedCallArgCount {
                expected_callee_arg_count,
                found_arg_count,
                found_expr,
            } => {
                let text_range = source_map
                    .source_by_expr(db)
                    .get(found_expr)
                    .unwrap()
                    .value
                    .node
                    .text_range();
                let expected_callee_arg_count = *expected_callee_arg_count;
                let found_arg_count = *found_arg_count;

                Diagnostic {
                    file,
                    title: "Mismatched argument count in call".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!(
                            "expected {expected_callee_arg_count} argument, found: {found_arg_count}"
                        ),
                        range: (text_range.start().into())..(text_range.end().into()),
                    }],
                }
            }
            InferenceError::NotCallable {
                found_callee_ty,
                found_callee_symbol: _,
                found_callee_expr,
            } => {
                let text_range = found_callee_expr.text_range(db, source_map);
                let found_callee_ty = type_to_string(db, found_callee_ty);

                Diagnostic {
                    file,
                    title: "Not callable".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected <function>, found: {found_callee_ty}"),
                        range: (text_range.start().into())..(text_range.end().into()),
                    }],
                }
            }
            InferenceError::ModuleAsExpr {
                found_module: _,
                found_expr,
            } => {
                let text_range = found_expr.text_range(db, source_map);

                Diagnostic {
                    file,
                    title: "Module as expression".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: "expected <expression>".to_string(),
                        range: (text_range.start().into())..(text_range.end().into()),
                    }],
                }
            }
        }
    }
}
#[derive(Debug)]
struct Message {
    range: std::ops::Range<usize>,
    message: String,
}

fn print_error(
    db: &base_db::SalsaDatabase,
    file_db: &hir::HirFileDatabase,
    source_map: &hir::HirFileSourceMap,
    error: &hir_ty::InferenceError,
    write_dest_err: &mut impl std::io::Write,
    config: ariadne::Config,
) {
    let diagnostic = Diagnostic::from_error(db, file_db, source_map, error);
    let file = diagnostic.file;
    let file_path = file.file_path(db).to_str().unwrap().to_string();
    let labels = diagnostic
        .messages
        .into_iter()
        .map(|message| Label::new((&file_path, message.range)).with_message(message.message));

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
