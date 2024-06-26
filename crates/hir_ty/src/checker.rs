use indexmap::IndexMap;

use crate::inference::{InferenceBodyResult, InferenceResult, Monotype};

mod mutability;

pub fn check_type_pods(
    db: &dyn hir::HirMasterDatabase,
    pods: &hir::Pods,
    infer_result: &InferenceResult,
) -> TypeCheckResult {
    // TODO: 全てのPodをチェックする
    let pod = &pods.root_pod;
    let mut errors_by_function = IndexMap::new();

    for (hir_file, function) in pod.all_functions(db) {
        let type_checker = FunctionTypeChecker::new(db, infer_result, hir_file, function);
        let type_errors = type_checker.check();
        errors_by_function.insert(function, type_errors);
    }

    for (hir_file, function) in pod.all_functions(db) {
        let mutability_checker =
            mutability::FunctionMutabilityChecker::new(db, pod, hir_file, function);
        let error = mutability_checker.check();
        errors_by_function
            .entry(function)
            .or_insert_with(Vec::new)
            .extend(error);
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
    ImmutableReassignment {
        expr: hir::ExprId,
    },
}

/// 型チェックの結果
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeCheckResult {
    /// 関数別の型チェックエラー一覧
    pub errors_by_function: IndexMap<hir::Function, Vec<TypeCheckError>>,
}

struct FunctionTypeChecker<'a> {
    db: &'a dyn hir::HirMasterDatabase,
    infer_result: &'a InferenceResult,

    hir_file: hir::HirFile,
    hir_file_db: &'a hir::HirFileDatabase,
    function: hir::Function,

    errors: Vec<TypeCheckError>,
}

impl<'a> FunctionTypeChecker<'a> {
    fn new(
        db: &'a dyn hir::HirMasterDatabase,
        infer_result: &'a InferenceResult,

        hir_file: hir::HirFile,
        function: hir::Function,
    ) -> Self {
        Self {
            db,
            infer_result,
            hir_file,
            hir_file_db: hir_file.db(db),
            function,
            errors: vec![],
        }
    }

    fn check(mut self) -> Vec<TypeCheckError> {
        let body = self.function.body(self.db, self.hir_file).unwrap();

        match body {
            hir::Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.check_stmt(stmt);
                }
                if let Some(tail) = block.tail {
                    self.check_expr(tail);
                }
            }
            _ => unreachable!(),
        }

        self.errors
    }

    fn check_stmt(&mut self, stmt: &'a hir::Stmt) {
        match stmt {
            hir::Stmt::Expr { expr, .. } => self.check_expr(*expr),
            hir::Stmt::Let { value, .. } => self.check_expr(*value),
            hir::Stmt::Item { .. } => (),
        }
    }

    fn check_expr(&mut self, expr: hir::ExprId) {
        let Some(ty) = self.current_inference().type_by_expr(expr) else {
            return;
        };
        if ty == &Monotype::Unknown {
            self.errors.push(TypeCheckError::UnresolvedType { expr });
        }

        let expr = expr.lookup(self.hir_file_db);
        match expr {
            hir::Expr::Symbol(_) => (),
            hir::Expr::Binary { lhs, rhs, .. } => {
                self.check_expr(*lhs);
                self.check_expr(*rhs);
            }
            hir::Expr::Unary { expr, .. } => {
                self.check_expr(*expr);
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
                for arg in args {
                    self.check_expr(*arg);
                }

                match callee {
                    hir::Symbol::Local { binding, .. } => {
                        let expr = binding.lookup(self.hir_file_db).expr;
                        self.check_expr(expr);
                    }
                    hir::Symbol::Param { .. }
                    | hir::Symbol::MissingExpr { .. }
                    | hir::Symbol::MissingType { .. } => (),
                };
            }
            hir::Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.check_expr(*condition);

                self.check_expr(*then_branch);
                if let Some(else_branch) = else_branch {
                    self.check_expr(*else_branch);
                }
            }
            hir::Expr::Return { value } => {
                if let Some(value) = value {
                    self.check_expr(*value);
                }
            }
            hir::Expr::Loop { block } => {
                self.check_expr(*block);
            }
            hir::Expr::Continue => (),
            hir::Expr::Break { value } => {
                if let Some(value) = value {
                    self.check_expr(*value);
                }
            }
            hir::Expr::Record { fields, .. } => {
                for field in fields {
                    self.check_expr(field.value);
                }
            }
            hir::Expr::Field { base, .. } => {
                self.check_expr(*base);
            }
            hir::Expr::Missing => (),
        };
    }

    /// 現在の関数の推論結果を取得する
    fn current_inference(&self) -> &InferenceBodyResult {
        self.infer_result
            .inference_body_result_by_function
            .get(&self.function)
            .unwrap()
    }
}
