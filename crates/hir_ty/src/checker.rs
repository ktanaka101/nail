use hir::LowerResult;

use crate::inference::{InferenceResult, ResolvedType, Signature};

pub fn check_type(lower_result: &LowerResult, infer_result: &InferenceResult) -> TypeCheckResult {
    let type_checker = TypeChecker::new(lower_result, infer_result);
    type_checker.check()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeCheckError {
    UnresolvedType {
        expr: hir::ExprId,
    },
    MismatchedTypes {
        expected_expr: hir::ExprId,
        expected_ty: ResolvedType,
        found_expr: hir::ExprId,
        found_ty: ResolvedType,
    },
    MismatchedTypeIfCondition {
        expected_ty: ResolvedType,
        found_expr: hir::ExprId,
        found_ty: ResolvedType,
    },
    MismatchedTypeElseBranch {
        expected_ty: ResolvedType,
        found_expr: hir::ExprId,
        found_ty: ResolvedType,
    },
    MismaatchedSignature {
        expected_ty: ResolvedType,
        signature: Signature,
        found_expr: hir::ExprId,
        found_ty: ResolvedType,
    },
    MismatchedReturnType {
        expected_ty: ResolvedType,
        found_expr: Option<hir::ExprId>,
        found_ty: ResolvedType,
    },
}

pub struct TypeCheckResult {
    pub errors: Vec<TypeCheckError>,
}

struct TypeChecker<'a> {
    lower_result: &'a LowerResult,
    infer_result: &'a InferenceResult,
    errors: Vec<TypeCheckError>,
    current_function: Option<hir::FunctionId>,
}

impl<'a> TypeChecker<'a> {
    fn new(lower_result: &'a LowerResult, infer_result: &'a InferenceResult) -> Self {
        Self {
            lower_result,
            infer_result,
            errors: vec![],
            current_function: None,
        }
    }

    fn check(mut self) -> TypeCheckResult {
        for (id, _) in self.lower_result.db.functions() {
            self.check_function(id);
        }

        TypeCheckResult {
            errors: self.errors,
        }
    }

    fn set_function(&mut self, function_id: hir::FunctionId) {
        self.current_function = Some(function_id);
    }

    fn check_function(&mut self, function_id: hir::FunctionId) {
        self.set_function(function_id);

        let block_ast_id = self
            .lower_result
            .item_tree
            .block_id_by_function(&function_id)
            .unwrap();
        let body = self
            .lower_result
            .shared_ctx
            .function_body_by_block(block_ast_id)
            .unwrap();

        let signature =
            &self.infer_result.signatures[self.infer_result.signature_by_function[&function_id]];
        match body {
            hir::Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.check_stmt(stmt);
                }
                if let Some(tail) = block.tail {
                    self.check_expr(tail);

                    let tail_ty = self.infer_result.type_by_expr[&tail];
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
    }

    fn check_stmt(&mut self, stmt: &hir::Stmt) {
        match stmt {
            hir::Stmt::ExprStmt { expr, .. } => self.check_expr(*expr),
            hir::Stmt::VariableDef { value, .. } => self.check_expr(*value),
            hir::Stmt::Item { .. } => (),
        }
    }

    fn check_expr(&mut self, expr: hir::ExprId) {
        let expr = expr.lookup(&self.lower_result.shared_ctx);
        match expr {
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
                    hir::Symbol::Function { function, .. } => {
                        Some(self.infer_result.signature_by_function[function])
                    }
                    _ => None,
                };
                if let Some(signature) = signature {
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
            }
            hir::Expr::VariableRef { .. } => (),
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

                let signature = self
                    .infer_result
                    .signature_by_function
                    .get(&self.current_function.unwrap())
                    .unwrap();
                let signature = &self.infer_result.signatures[*signature];

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
        let ty = self.infer_result.type_by_expr[&expr];
        if ty == ResolvedType::Unknown {
            self.errors.push(TypeCheckError::UnresolvedType { expr });
        }

        ty
    }
}
