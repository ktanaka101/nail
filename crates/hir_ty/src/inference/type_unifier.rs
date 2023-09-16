use std::collections::HashMap;

use super::{error::InferenceError, types::Monotype, Signature};

#[derive(Default, Debug)]
pub(crate) struct TypeUnifier {
    pub(crate) nodes: HashMap<Monotype, Node>,
    pub(crate) errors: Vec<InferenceError>,
}

/// 型推論の目的
///
/// エラーを具体的に収集するために使用されます。
pub(crate) enum UnifyPurpose {
    CallArg {
        /// 関数呼び出し対象のシグネチャ
        callee_signature: Signature,
        /// 引数の式
        found_arg: hir::ExprId,
    },
    BinaryInteger {
        /// 数値演算子の対象式
        found_expr: hir::ExprId,
        /// 演算子
        op: ast::BinaryOp,
    },
    BinaryCompare {
        /// 比較元の式
        found_compare_from_expr: hir::ExprId,
        /// 比較先の式
        found_compare_to_expr: hir::ExprId,
        /// 演算子
        op: ast::BinaryOp,
    },
    Unary {
        /// 単行演算子の対象式
        found_expr: hir::ExprId,
        /// 演算子
        op: ast::UnaryOp,
    },
    IfCondition {
        /// If条件の式
        found_condition_expr: hir::ExprId,
    },
    IfThenElseBranch {
        /// Thenブランチの式
        found_then_branch_expr: hir::ExprId,
        /// Elseブランチの式
        found_else_branch_expr: hir::ExprId,
    },
    IfThenOnlyBranch {
        /// Thenブランチの式
        found_then_branch_expr: hir::ExprId,
    },
    ReturnValue {
        /// 期待する関数のシグネチャ
        expected_signature: Signature,
        /// 戻り値の式
        ///
        /// Noneの場合は`return;`のように指定なしを表す
        found_return_expr: Option<hir::ExprId>,
    },
}

/// 型の不一致を表すエラーを生成します。
fn build_unify_error_from_unify_purpose(
    expected_ty: Monotype,
    found_ty: Monotype,
    purpose: &UnifyPurpose,
) -> InferenceError {
    match purpose {
        UnifyPurpose::CallArg {
            found_arg,
            callee_signature: expected_signature,
        } => InferenceError::MismaatchedSignature {
            expected_ty,
            found_ty,
            signature: *expected_signature,
            found_expr: *found_arg,
        },
        UnifyPurpose::BinaryInteger { found_expr, op } => InferenceError::MismatchedBinaryInteger {
            expected_int_ty: expected_ty,
            found_ty,
            found_expr: *found_expr,
            op: op.clone(),
        },
        UnifyPurpose::BinaryCompare {
            found_compare_from_expr,
            found_compare_to_expr,
            op,
        } => InferenceError::MismatchedBinaryCompare {
            compare_from_ty: expected_ty,
            compare_to_ty: found_ty,
            compare_from_expr: *found_compare_from_expr,
            compare_to_expr: *found_compare_to_expr,
            op: op.clone(),
        },
        UnifyPurpose::Unary { found_expr, op } => InferenceError::MismatchedUnary {
            expected_ty,
            found_ty,
            found_expr: *found_expr,
            op: op.clone(),
        },
        UnifyPurpose::IfCondition {
            found_condition_expr,
        } => InferenceError::MismatchedTypeIfCondition {
            expected_condition_bool_ty: expected_ty,
            found_condition_ty: found_ty,
            found_condition_expr: *found_condition_expr,
        },
        UnifyPurpose::IfThenElseBranch {
            found_then_branch_expr,
            found_else_branch_expr,
        } => InferenceError::MismatchedTypeElseBranch {
            then_branch_ty: expected_ty,
            then_branch: *found_then_branch_expr,
            else_branch_ty: found_ty,
            else_branch: *found_else_branch_expr,
        },
        UnifyPurpose::IfThenOnlyBranch {
            found_then_branch_expr,
        } => InferenceError::MismatchedTypeOnlyIfBranch {
            then_branch_ty: found_ty,
            then_branch: *found_then_branch_expr,
            else_branch_unit_ty: expected_ty,
        },
        UnifyPurpose::ReturnValue {
            expected_signature,
            found_return_expr,
        } => InferenceError::MismatchedTypeReturnValue {
            found_ty,
            found_return_expr: *found_return_expr,
            expected_signature: *expected_signature,
        },
    }
}

impl TypeUnifier {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn find(&mut self, ty: &Monotype) -> Monotype {
        let node = self.nodes.get(ty);
        if let Some(node) = node {
            node.topmost_parent().value
        } else {
            self.nodes.insert(ty.clone(), Node::new(ty.clone()));
            ty.clone()
        }
    }

    pub fn unify(&mut self, a_expected: &Monotype, b_actual: &Monotype, purpose: &UnifyPurpose) {
        let a_rep = self.find(a_expected);
        let b_rep = self.find(b_actual);

        if a_rep == b_rep {
            return;
        }

        match (&a_rep, &b_rep) {
            (Monotype::Function(_a_signature), Monotype::Function(_b_signature)) => {
                unreachable!();
                // self.unify(&a_signature.return_type, &b_signature.return_type, purpose);

                // if a_signature.params.len() != b_signature.params.len() {
                //     // エラーを追加する
                //     return;
                // }

                // for (a_arg, b_arg) in a_signature.params.iter().zip(b_signature.params.iter()) {
                //     self.unify(a_arg, b_arg, purpose);
                // }
            }
            (Monotype::Variable(_), b_rep) => self.unify_var(b_rep, &a_rep),
            (a_rep, Monotype::Variable(_)) => self.unify_var(a_rep, &b_rep),
            // 実際の型がNever型の場合は、期待する型がなんであれ到達しないので型チェックする必要がない
            // 期待する型がNever型の場合は、、値が渡ってはいけないので型チェックするべき。途中でreturnした場合など改善の余地があると思われる。
            (_, Monotype::Never) => (),
            (_, _) => {
                self.errors
                    .push(build_unify_error_from_unify_purpose(a_rep, b_rep, purpose));
            }
        }
    }

    fn unify_var(&mut self, term: &Monotype, type_var: &Monotype) {
        assert!(matches!(type_var, Monotype::Variable(_)));

        let value = Some(Box::new(self.nodes.get(term).unwrap().clone()));
        let node = self.nodes.get_mut(type_var);
        node.unwrap().parent = value;
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    value: Monotype,
    parent: Option<Box<Node>>,
}

impl Node {
    fn new(ty: Monotype) -> Self {
        Node {
            value: ty,
            parent: None,
        }
    }

    fn topmost_parent(&self) -> Self {
        if let Some(node) = &self.parent {
            node.topmost_parent()
        } else {
            self.clone()
        }
    }
}
