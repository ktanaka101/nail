use std::collections::HashMap;

use la_arena::Idx;

use crate::inference::{InferenceBodyResult, InferenceResult, ResolvedType, Signature};

pub fn check_type_pods(
    db: &dyn hir::HirMasterDatabase,
    pods: &hir::Pods,
    infer_result: &InferenceResult,
) -> TypeCheckResult {
    // TODO: 全てのPodをチェックする
    let pod = &pods.pods[0];
    let mut errors_by_function = HashMap::new();

    for (hir_file, function) in pod.all_functions(db) {
        let type_checker =
            FunctionTypeChecker::new(db, &pods.resolution_map, infer_result, hir_file, function);
        let type_errors = type_checker.check();
        errors_by_function.insert(function, type_errors);
    }

    TypeCheckResult { errors_by_function }
}

/// 型チェックのエラー
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeCheckError {
    /// 型を解決できない
    UnresolvedType {
        /// 対象の式
        expr: hir::ExprId,
    },
    /// 一致するべき型が一致しない
    MismatchedTypes {
        /// 期待される型の式
        expected_expr: hir::ExprId,
        /// 期待される型
        expected_ty: ResolvedType,
        /// 実際の式
        found_expr: hir::ExprId,
        /// 実際の型
        found_ty: ResolvedType,
    },
    /// Ifの条件式の型が一致しない
    MismatchedTypeIfCondition {
        /// 期待される型
        expected_ty: ResolvedType,
        /// 実際の式
        found_expr: hir::ExprId,
        /// 実際の型
        found_ty: ResolvedType,
    },
    /// Ifのthenブランチとelseブランチの型が一致しない
    MismatchedTypeElseBranch {
        /// 期待される型
        expected_ty: ResolvedType,
        /// 実際の式
        found_expr: hir::ExprId,
        /// 実際の型
        found_ty: ResolvedType,
    },
    /// 関数呼び出しの引数の数が一致しない
    MismaatchedSignature {
        /// 期待される型
        expected_ty: ResolvedType,
        /// 呼び出そうとしている関数のシグネチャ
        signature: Signature,
        /// 実際の式
        found_expr: hir::ExprId,
        /// 実際の型
        found_ty: ResolvedType,
    },
    /// 関数の戻り値の型と実際の戻り値の型が異なる
    ///
    /// 以下のいずれかが関数の戻り値の型と一致しない場合に発生する
    /// - `return`に指定した式の型
    /// - 関数ボディの最後の式の型
    MismatchedReturnType {
        /// 期待される型
        expected_ty: ResolvedType,
        /// 実際の式
        found_expr: Option<hir::ExprId>,
        /// 実際の型
        found_ty: ResolvedType,
    },
}

/// 型チェックの結果
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeCheckResult {
    /// 関数別の型チェックエラー一覧
    pub errors_by_function: HashMap<hir::Function, Vec<TypeCheckError>>,
}

struct FunctionTypeChecker<'a> {
    db: &'a dyn hir::HirMasterDatabase,
    resolution_map: &'a hir::ResolutionMap,
    infer_result: &'a InferenceResult,

    hir_file: hir::HirFile,
    function: hir::Function,

    errors: Vec<TypeCheckError>,
}

impl<'a> FunctionTypeChecker<'a> {
    fn new(
        db: &'a dyn hir::HirMasterDatabase,
        resolution_map: &'a hir::ResolutionMap,
        infer_result: &'a InferenceResult,

        hir_file: hir::HirFile,
        function: hir::Function,
    ) -> Self {
        Self {
            db,
            resolution_map,
            infer_result,
            hir_file,
            function,
            errors: vec![],
        }
    }

    fn signature_by_function(&self, function: hir::Function) -> Idx<Signature> {
        self.infer_result.signature_by_function[&function]
    }

    fn check(mut self) -> Vec<TypeCheckError> {
        let block_ast_id = self.function.ast(self.db).body().unwrap();
        let body = self
            .hir_file
            .db(self.db)
            .function_body_by_ast_block(block_ast_id)
            .unwrap();

        match body {
            hir::Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.check_stmt(stmt);
                }
                if let Some(tail) = block.tail {
                    self.check_expr(tail);

                    let signature = self.signature_by_function(self.function);
                    let signature = &self.infer_result.signatures[signature];
                    let tail_ty = self.current_inference().type_by_expr[&tail];
                    if tail_ty != signature.return_type {
                        self.errors.push(TypeCheckError::MismatchedReturnType {
                            expected_ty: signature.return_type,
                            found_expr: Some(tail),
                            found_ty: tail_ty,
                        });
                    }
                } else {
                }
            }
            _ => unreachable!(),
        }

        self.errors
    }

    fn check_stmt(&mut self, stmt: &'a hir::Stmt) {
        match stmt {
            hir::Stmt::ExprStmt { expr, .. } => self.check_expr(*expr),
            hir::Stmt::VariableDef { value, .. } => self.check_expr(*value),
            hir::Stmt::Item { .. } => (),
        }
    }

    fn check_expr(&mut self, expr: hir::ExprId) {
        let expr = expr.lookup(self.hir_file.db(self.db));
        match expr {
            hir::Expr::Symbol(_) => (),
            hir::Expr::Binary { lhs, rhs, .. } => {
                let lhs_ty = self.type_by_expr(*lhs);
                let rhs_ty = self.type_by_expr(*rhs);
                match (lhs_ty, rhs_ty) {
                    (ResolvedType::Unknown, ResolvedType::Unknown) => (),
                    (lhs_ty, rhs_ty) => {
                        if lhs_ty != rhs_ty {
                            self.errors.push(TypeCheckError::MismatchedTypes {
                                expected_expr: *lhs,
                                expected_ty: lhs_ty,
                                found_expr: *rhs,
                                found_ty: rhs_ty,
                            });
                        }
                    }
                }
            }
            hir::Expr::Unary { expr, .. } => {
                self.type_by_expr(*expr);
            }
            hir::Expr::Literal(_) => (),
            hir::Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.check_stmt(stmt);
                }
                if let Some(tail) = block.tail {
                    self.check_expr(tail);
                }
            }
            hir::Expr::Call { callee, args } => {
                let signature = match callee {
                    hir::Symbol::Local { .. } | hir::Symbol::Param { .. } => return,
                    hir::Symbol::Missing { path } => {
                        let Some(item) = self.resolution_map.item_by_symbol(path) else { return; };
                        match item {
                            hir::ResolutionStatus::Unresolved | hir::ResolutionStatus::Error => {
                                return
                            }
                            hir::ResolutionStatus::Resolved { path: _, item } => match item {
                                hir::Item::Function(function) => {
                                    self.signature_by_function(function)
                                }
                                hir::Item::Module(_) | hir::Item::UseItem(_) => unimplemented!(),
                            },
                        }
                    }
                };

                let signature = &self.infer_result.signatures[signature];
                for (i, param_ty) in signature.params.iter().enumerate() {
                    let arg = args[i];
                    let arg_ty = self.type_by_expr(arg);
                    if *param_ty != arg_ty {
                        self.errors.push(TypeCheckError::MismaatchedSignature {
                            expected_ty: *param_ty,
                            signature: signature.clone(),
                            found_expr: arg,
                            found_ty: arg_ty,
                        });
                    }
                }
            }
            hir::Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_ty = self.type_by_expr(*condition);
                let expected_condition_ty = ResolvedType::Bool;
                if condition_ty != expected_condition_ty {
                    self.errors.push(TypeCheckError::MismatchedTypeIfCondition {
                        expected_ty: expected_condition_ty,
                        found_expr: *condition,
                        found_ty: condition_ty,
                    });
                }

                let then_branch_ty = self.type_by_expr(*then_branch);
                if let Some(else_branch) = else_branch {
                    let else_branch_ty = self.type_by_expr(*else_branch);
                    if then_branch_ty != else_branch_ty {
                        self.errors.push(TypeCheckError::MismatchedTypes {
                            expected_expr: *then_branch,
                            expected_ty: then_branch_ty,
                            found_expr: *else_branch,
                            found_ty: else_branch_ty,
                        });
                    }
                } else {
                    let else_branch_ty = ResolvedType::Unit;
                    if then_branch_ty != else_branch_ty {
                        self.errors.push(TypeCheckError::MismatchedTypeElseBranch {
                            expected_ty: else_branch_ty,
                            found_expr: *then_branch,
                            found_ty: then_branch_ty,
                        });
                    }
                }
            }
            hir::Expr::Return { value } => {
                let return_value_ty = if let Some(value) = value {
                    self.type_by_expr(*value)
                } else {
                    ResolvedType::Unit
                };

                let signature = self.signature_by_function(self.function);
                let signature = &self.infer_result.signatures[signature];
                if return_value_ty != signature.return_type {
                    self.errors.push(TypeCheckError::MismatchedReturnType {
                        expected_ty: signature.return_type,
                        found_expr: *value,
                        found_ty: return_value_ty,
                    });
                }
            }
            hir::Expr::Missing => (),
        }
    }

    fn type_by_expr(&mut self, expr: hir::ExprId) -> ResolvedType {
        let ty = self.current_inference().type_by_expr[&expr];
        if ty == ResolvedType::Unknown {
            self.errors.push(TypeCheckError::UnresolvedType { expr });
        }

        ty
    }

    /// 現在の関数の推論結果を取得する
    fn current_inference(&self) -> &InferenceBodyResult {
        self.infer_result
            .inference_by_body
            .get(&self.function)
            .unwrap()
    }
}
