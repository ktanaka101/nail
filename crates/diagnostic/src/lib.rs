//! Define the content of error output for users.
//! For example, used in LSP and Dock.

use ast::AstNode;

/// 診断情報
/// 基本的には、1つのエラーから1つの診断情報が作成されます。そのため、同じファイル
/// 場合によっては、1つのエラーから1つの診断情報の中に複数のメッセージが含まれる場合があります。
#[derive(Debug)]
pub struct Diagnostic {
    /// 診断情報対象のファイル
    pub file: hir::NailFile,
    /// 診断情報のタイトル(サマリ)
    pub title: String,
    /// 診断情報の開始位置
    /// メッセージ一覧内の中で一番小さい開始位置です。
    pub head_offset: usize,
    /// 診断情報のメッセージ一覧
    pub messages: Vec<Message>,
}
impl Diagnostic {
    /// parser::ParserErrorの情報を元に診断情報を作成します。
    pub fn from_parse_error(file: hir::NailFile, error: &parser::ParserError) -> Diagnostic {
        match error {
            parser::ParserError::ParseError(err) => {
                let text_range = err.range();
                Diagnostic {
                    file,
                    title: "syntax error".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("{}", err),
                        range: text_range,
                    }],
                }
            }
            parser::ParserError::TokenError(err) => {
                let text_range = err.range();
                Diagnostic {
                    file,
                    title: "token error".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("{}", err),
                        range: text_range,
                    }],
                }
            }
        }
    }

    /// ast::validation::ValidationErrorの情報を元に診断情報を作成します。
    pub fn from_validation_error(
        file: hir::NailFile,
        error: &ast::validation::ValidationError,
    ) -> Diagnostic {
        let text_range = error.range();
        Diagnostic {
            file,
            title: "validation error".to_string(),
            head_offset: text_range.start().into(),
            messages: vec![Message {
                message: format!("{}", error),
                range: text_range,
            }],
        }
    }

    /// hir::LowerErrorの情報を元に診断情報を作成します。
    pub fn from_hir_lower_error(
        db: &base_db::SalsaDatabase,
        source_map: &hir::HirFileSourceMap,
        error: &hir::LowerError,
    ) -> Self {
        let file = source_map.file(db);
        match error {
            hir::LowerError::UndefinedEntryPoint => Diagnostic {
                file,
                title: "Undefined entry point".to_string(),
                head_offset: 0,
                messages: vec![Message {
                    message: "entry point is not defined".to_string(),
                    range: ast::TextRange::new(0.into(), 0.into()),
                }],
            },
        }
    }

    /// hir_tyの型推論エラー情報を元に診断情報を作成します。
    pub fn from_hir_ty_inference_error(
        db: &base_db::SalsaDatabase,
        hir_file: hir::HirFile,
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
                let cond_type = monotype_to_string(db, expected_condition_bool_ty);
                let found_cond_ty = monotype_to_string(db, found_condition_ty);

                Diagnostic {
                    file,
                    title: "Mismatched type in if condition".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected {cond_type}, actual: {found_cond_ty}"),
                        range: text_range,
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
                let hir::Expr::Block(then_block) = then_branch.lookup(file_db) else {
                    unreachable!()
                };
                let then_tail_range = if let Some(tail) = then_block.tail {
                    tail.text_range(db, source_map)
                } else {
                    ast::TextRange::new(then_branch_range.end(), then_branch_range.end())
                };
                let hir::Expr::Block(else_block) = else_branch.lookup(file_db) else {
                    unreachable!()
                };
                let else_tail_range = if let Some(tail) = else_block.tail {
                    tail.text_range(db, source_map)
                } else {
                    ast::TextRange::new(else_branch_range.end(), else_branch_range.end())
                };

                let then_branch_ty = monotype_to_string(db, then_branch_ty);
                let else_branch_ty = monotype_to_string(db, else_branch_ty);

                Diagnostic {
                    file,
                    title: "Mismatched type if branch and else branch".to_string(),
                    head_offset: then_branch_range.start().into(),
                    messages: vec![
                        Message {
                            message: format!("expected {then_branch_ty}, actual: {else_branch_ty}"),
                            range: ast::TextRange::new(
                                then_branch_range.start(),
                                else_branch_range.start(),
                            ),
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
                let hir::Expr::Block(then_block) = then_branch.lookup(file_db) else {
                    unreachable!()
                };
                let then_tail_range = if let Some(tail) = then_block.tail {
                    tail.text_range(db, source_map)
                } else {
                    unreachable!("末尾の式がない場合は必ずUnitなのでエラーとならないはずです。");
                };

                let then_branch_ty = monotype_to_string(db, then_branch_ty);
                let else_branch_ty = monotype_to_string(db, else_branch_unit_ty);

                Diagnostic {
                    file,
                    title: "Mismatched type if branch and else branch".to_string(),
                    head_offset: then_branch_range.start().into(),
                    messages: vec![
                        Message {
                            message: format!("expected {else_branch_ty}, actual {then_branch_ty}"),
                            range: then_branch_range,
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
                let expected_ty = monotype_to_string(db, expected_ty);
                let found_ty = monotype_to_string(db, found_ty);

                Diagnostic {
                    file,
                    title: "Mismatched type in call argument".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected {expected_ty}, actual: {found_ty}"),
                        range: text_range,
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
                let expected_ty = monotype_to_string(db, expected_int_ty);
                let found_ty = monotype_to_string(db, found_ty);

                let op = match op {
                    hir::BinaryOp::Add => "+",
                    hir::BinaryOp::Sub => "-",
                    hir::BinaryOp::Mul => "*",
                    hir::BinaryOp::Div => "/",
                    hir::BinaryOp::Equal
                    | hir::BinaryOp::NotEq
                    | hir::BinaryOp::GreaterThan
                    | hir::BinaryOp::LessThan
                    | hir::BinaryOp::GtEq
                    | hir::BinaryOp::LtEq
                    | hir::BinaryOp::Assign => unreachable!(),
                };

                Diagnostic {
                    file,
                    title: format!("Mismatched type in binary integer expression `{op}`"),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected {expected_ty}, actual: {found_ty}"),
                        range: text_range,
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
                let compare_from_ty = monotype_to_string(db, compare_from_ty);
                let compare_to_ty = monotype_to_string(db, compare_to_ty);

                let op = match op {
                    hir::BinaryOp::Equal => "==",
                    hir::BinaryOp::NotEq => "!=",
                    hir::BinaryOp::GreaterThan => ">",
                    hir::BinaryOp::LessThan => "<",
                    hir::BinaryOp::GtEq => ">=",
                    hir::BinaryOp::LtEq => "<=",
                    hir::BinaryOp::Add
                    | hir::BinaryOp::Sub
                    | hir::BinaryOp::Mul
                    | hir::BinaryOp::Div
                    | hir::BinaryOp::Assign => unreachable!(),
                };

                Diagnostic {
                    file,
                    title: format!("Mismatched type in binary compare expression `{op}`"),
                    head_offset: compare_from_range.start().into(),
                    messages: vec![
                        Message {
                            range: compare_from_range,
                            message: format!("Type is {compare_from_ty}"),
                        },
                        Message {
                            range: compare_to_range,
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
                let expected_ty = monotype_to_string(db, expected_ty);
                let found_ty = monotype_to_string(db, found_ty);

                let op = match op {
                    hir::UnaryOp::Not => "!",
                    hir::UnaryOp::Neg => "-",
                };

                Diagnostic {
                    file,
                    title: format!("Mismatched type in unary expression `{op}`"),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected {expected_ty}, actual: {found_ty}"),
                        range: text_range,
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
                let return_type = monotype_to_string(db, &return_type);
                let found_ty = monotype_to_string(db, found_ty);

                Diagnostic {
                    file,
                    title: "Mismatched type in return expression".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected {return_type}, actual: {found_ty}"),
                        range: text_range,
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
                    let return_type = monotype_to_string(db, &return_type);
                    let found_ty = monotype_to_string(db, found_ty);

                    Diagnostic {
                        file,
                        title: "Mismatched type in return value".to_string(),
                        head_offset: text_range.start().into(),
                        messages: vec![Message {
                            message: format!("expected {return_type}, actual: {found_ty}"),
                            range: text_range,
                        }],
                    }
                } else {
                    let text_range = expected_function
                        .ast_def(db, hir_file)
                        .unwrap()
                        .return_type()
                        .unwrap()
                        .syntax()
                        .text_range();

                    let return_type = expected_signature.return_type(db);
                    let return_type = monotype_to_string(db, &return_type);
                    let found_ty = monotype_to_string(db, found_ty);

                    Diagnostic {
                        file,
                        title: "Mismatched type in return value".to_string(),
                        head_offset: text_range.start().into(),
                        messages: vec![Message {
                            message: format!(
                                "expected {return_type}, actual: {found_ty} as its body has tail"
                            ),
                            range: text_range,
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
                        range: text_range,
                    }],
                }
            }
            InferenceError::NotCallable {
                found_callee_ty,
                found_callee_symbol: _,
                found_callee_expr,
            } => {
                let text_range = found_callee_expr.text_range(db, source_map);
                let found_callee_ty = monotype_to_string(db, found_callee_ty);

                Diagnostic {
                    file,
                    title: "Not callable".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected <function>, found: {found_callee_ty}"),
                        range: text_range,
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
                        range: text_range,
                    }],
                }
            }
            InferenceError::MismatchedType {
                expected_ty,
                expected_expr,
                found_ty,
                found_expr,
            } => {
                let found_expr_text_range = found_expr.text_range(db, source_map);
                let expected_ty = monotype_to_string(db, expected_ty);
                let found_ty = monotype_to_string(db, found_ty);

                if let Some(expected_expr) = expected_expr {
                    let expected_expr_text_range = expected_expr.text_range(db, source_map);
                    Diagnostic {
                        file,
                        title: "Mismatched type".to_string(),
                        head_offset: expected_expr_text_range.start().into(),
                        messages: vec![
                            Message {
                                message: format!("Type is {expected_ty}"),
                                range: expected_expr_text_range,
                            },
                            Message {
                                message: format!("Type is {found_ty}"),
                                range: found_expr_text_range,
                            },
                        ],
                    }
                } else {
                    Diagnostic {
                        file,
                        title: "Mismatched type".to_string(),
                        head_offset: found_expr_text_range.start().into(),
                        messages: vec![Message {
                            message: format!("expected {expected_ty}, actual: {found_ty}"),
                            range: found_expr_text_range,
                        }],
                    }
                }
            }
            InferenceError::BreakOutsideOfLoop { kind, found_expr } => {
                let text_range = found_expr.text_range(db, source_map);
                let kind_text = match kind {
                    hir_ty::BreakKind::Break => "Break",
                    hir_ty::BreakKind::Continue => "Continue",
                };

                Diagnostic {
                    file,
                    title: format!("{kind_text} outside of loop"),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: "expected in <loop>".to_string(),
                        range: text_range,
                    }],
                }
            }
            InferenceError::NotRecord {
                found_struct_ty,
                found_struct_symbol: _,
                found_expr,
            } => {
                let text_range = found_expr.text_range(db, source_map);
                let found_struct_style = match found_struct_ty {
                    hir_ty::Monotype::Struct(struct_) => match struct_.kind(db) {
                        hir::StructKind::Record(_) => unreachable!(),
                        hir::StructKind::Tuple(_) => "tuple style",
                        hir::StructKind::Unit => "unit style",
                    },
                    _ => unreachable!(),
                };

                Diagnostic {
                    file,
                    title: "Not record".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!(
                            "expected(defined): {found_struct_style}, found: record style"
                        ),
                        range: text_range,
                    }],
                }
            }
            InferenceError::NotAllowedType {
                found_symbol: _,
                found_function: _,
                found_ty,
            } => {
                let text_range = found_ty.text_range(db, source_map);
                Diagnostic {
                    file,
                    title: "Not allowed type".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: "not allowed type".to_string(),
                        range: text_range,
                    }],
                }
            }
            InferenceError::MismatchedTypeInitStructTuple {
                expected_ty,
                found_ty,
                found_arg_expr,
                arg_pos: _,
                init_struct: _,
                found_expr: _,
            } => {
                let text_range = found_arg_expr.text_range(db, source_map);
                let expected_ty = monotype_to_string(db, expected_ty);
                let found_ty = monotype_to_string(db, found_ty);

                Diagnostic {
                    file,
                    title: "Mismatched type in struct tuple initialization".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected {expected_ty}, actual: {found_ty}"),
                        range: text_range,
                    }],
                }
            }
            InferenceError::MissingStructRecordField {
                missing_fields,
                found_struct: _,
                found_expr,
            } => {
                let text_range = found_expr.text_range(db, source_map);
                let missing_fields = missing_fields
                    .iter()
                    .map(|field| field.text(db).to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                Diagnostic {
                    file,
                    title: "Missing struct record field".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("missing fields: {missing_fields}"),
                        range: text_range,
                    }],
                }
            }
            InferenceError::NoSuchStructRecordField {
                no_such_fields,
                found_struct: _,
                found_expr,
            } => {
                let text_range = found_expr.text_range(db, source_map);
                let no_such_fields = no_such_fields
                    .iter()
                    .map(|field| field.name.text(db).to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                Diagnostic {
                    file,
                    title: "No such struct record field".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("no such fields: {no_such_fields}"),
                        range: text_range,
                    }],
                }
            }
            InferenceError::MismatchedTypeInitStructRecord {
                expected_ty,
                found_ty,
                found_name: _,
                found_expr,
                found_struct: _,
            } => {
                let text_range = found_expr.text_range(db, source_map);
                let expected_ty = monotype_to_string(db, expected_ty);
                let found_ty = monotype_to_string(db, found_ty);

                Diagnostic {
                    file,
                    title: "Mismatched type in struct record initialization".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("expected {expected_ty}, actual: {found_ty}"),
                        range: text_range,
                    }],
                }
            }
            InferenceError::NeededInitTupleOrRecord {
                found_ty,
                found_expr,
                found_struct,
            } => {
                let text_range = found_expr.text_range(db, source_map);
                let found_ty = monotype_to_string(db, found_ty);
                let found_struct_style = match found_struct.kind(db) {
                    hir::StructKind::Record(_) => "record style",
                    hir::StructKind::Tuple(_) => "tuple style",
                    hir::StructKind::Unit => unreachable!(),
                };

                Diagnostic {
                    file,
                    title: "Needed initialization tuple or record".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!(
                            "expected {found_struct_style} initialization, found: {found_ty}"
                        ),
                        range: text_range,
                    }],
                }
            }
            InferenceError::NoSuchFieldAccess {
                no_such_field,
                found_struct,
                found_expr,
            } => {
                let text_range = found_expr.text_range(db, source_map);
                let no_such_field = no_such_field.text(db);
                let found_struct_name = found_struct.name(db).text(db);

                Diagnostic {
                    file,
                    title: "No such field access".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("no such field: {no_such_field} in {found_struct_name}"),
                        range: text_range,
                    }],
                }
            }
            InferenceError::CanNotFieldAccess {
                no_such_field,
                found_ty,
                found_expr,
            } => {
                let text_range = found_expr.text_range(db, source_map);
                let no_such_field = no_such_field.text(db);
                let found_ty = monotype_to_string(db, found_ty);

                Diagnostic {
                    file,
                    title: "Can not field access".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: format!("can not field access to {no_such_field} in {found_ty}"),
                        range: text_range,
                    }],
                }
            }
        }
    }

    /// hir_tyのチェックエラー情報を元に診断情報を作成します。
    pub fn from_hir_ty_check_error(
        db: &base_db::SalsaDatabase,
        _hir_file: hir::HirFile,
        _file_db: &hir::HirFileDatabase,
        source_map: &hir::HirFileSourceMap,
        error: &hir_ty::TypeCheckError,
    ) -> Self {
        match error {
            hir_ty::TypeCheckError::UnresolvedType { expr } => {
                let text_range = expr.text_range(db, source_map);
                Diagnostic {
                    file: source_map.file(db),
                    title: "Unresolved type".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: "cannot resolve type".to_string(),
                        range: text_range,
                    }],
                }
            }
            hir_ty::TypeCheckError::ImmutableReassignment { expr } => {
                let text_range = expr.text_range(db, source_map);
                Diagnostic {
                    file: source_map.file(db),
                    title: "Immutable reassignment".to_string(),
                    head_offset: text_range.start().into(),
                    messages: vec![Message {
                        message: "cannot reassign to immutable variable".to_string(),
                        range: text_range,
                    }],
                }
            }
        }
    }
}

/// 診断情報のメッセージ
#[derive(Debug)]
pub struct Message {
    /// メッセージの対象範囲
    pub range: ast::TextRange,
    /// メッセージの内容
    pub message: String,
}

fn monotype_to_string(db: &base_db::SalsaDatabase, ty: &hir_ty::Monotype) -> String {
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
                    .map(|param| monotype_to_string(db, param))
                    .collect::<Vec<String>>()
                    .join(", "),
                monotype_to_string(db, &signature.return_type(db))
            )
        }
        hir_ty::Monotype::Struct(struct_) => {
            let struct_name = struct_.name(db).text(db);
            format!("struct {}", struct_name)
        }
        hir_ty::Monotype::Char => "char".to_string(),
        hir_ty::Monotype::String => "string".to_string(),
        hir_ty::Monotype::Variable(_) => unreachable!(),
        hir_ty::Monotype::Never => "!".to_string(),
        hir_ty::Monotype::Unknown => "<unknown>".to_string(),
    }
}
