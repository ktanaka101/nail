use std::collections::HashMap;

use super::{error::InferenceError, types::Monotype, Signature};

#[derive(Default, Debug)]
pub(crate) struct TypeUnifier {
    pub(crate) nodes: HashMap<Monotype, Node>,
    pub(crate) errors: Vec<InferenceError>,
}

/// 型推論の目的
pub(crate) enum UnifyPurpose {
    Expr {
        /// 期待する型を持つ式
        expected_expr: hir::ExprId,
        /// 実際に得られた型を持つ式
        found_expr: hir::ExprId,
    },
    CallArg {
        /// 関数呼び出し対象のシグネチャ
        expected_signature: Signature,
        /// 実際に得られた型を持つ式
        found_arg: hir::ExprId,
    },
    SelfReturnType {
        expected_signature: Signature,
        found_expr: Option<hir::ExprId>,
    },
    BinaryInteger {
        /// 実際に得られた型を持つ式
        found_expr: hir::ExprId,
        /// 演算子
        op: ast::BinaryOp,
    },
    BinaryCompare {
        /// 期待する型を持つ式
        expected_expr: hir::ExprId,
        /// 実際に得られた型を持つ式
        found_expr: hir::ExprId,
        /// 演算子
        op: ast::BinaryOp,
    },
    Unary {
        /// 実際に得られた型を持つ式
        found_expr: hir::ExprId,
        /// 演算子
        op: ast::UnaryOp,
    },
    IfCondition {
        /// 実際に得られた型を持つ式
        found_expr: hir::ExprId,
    },
    IfThenElseBranch {
        /// 期待する型を持つ式
        expected_expr: hir::ExprId,
        /// 実際に得られた型を持つ式
        found_expr: hir::ExprId,
    },
    IfThenOnlyBranch {
        /// 実際に得られた型を持つ式
        found_expr: hir::ExprId,
    },
    ReturnValue {
        /// 期待する戻り値の型を持つ関数シグネチャ
        expected_signature: Signature,
        /// 実際に得られた型を持つ式
        found_expr: Option<hir::ExprId>,
    },
}

/// 型の不一致を表すエラーを生成します。
fn build_unify_error_from_unify_purpose(
    expected_ty: Monotype,
    found_ty: Monotype,
    purpose: &UnifyPurpose,
) -> InferenceError {
    match purpose {
        UnifyPurpose::Expr {
            expected_expr,
            found_expr,
        } => InferenceError::MismatchedTypes {
            expected_ty,
            found_ty,
            expected_expr: *expected_expr,
            found_expr: *found_expr,
        },
        UnifyPurpose::CallArg {
            found_arg,
            expected_signature,
        } => InferenceError::MismaatchedSignature {
            expected_ty,
            found_ty,
            signature: expected_signature.clone(),
            found_expr: *found_arg,
        },
        UnifyPurpose::SelfReturnType {
            expected_signature,
            found_expr,
        } => InferenceError::MismatchedTypeReturnValue {
            expected_signature: expected_signature.clone(),
            found_ty,
            found_expr: *found_expr,
        },
        UnifyPurpose::BinaryInteger { found_expr, op } => InferenceError::MismatchedBinaryInteger {
            expected_ty,
            found_ty,
            found_expr: *found_expr,
            op: op.clone(),
        },
        UnifyPurpose::BinaryCompare {
            expected_expr,
            found_expr,
            op,
        } => InferenceError::MismatchedBinaryCompare {
            expected_ty,
            found_ty,
            expected_expr: *expected_expr,
            found_expr: *found_expr,
            op: op.clone(),
        },
        UnifyPurpose::Unary { found_expr, op } => InferenceError::MismatchedUnary {
            expected_ty,
            found_ty,
            found_expr: *found_expr,
            op: op.clone(),
        },
        UnifyPurpose::IfCondition { found_expr } => InferenceError::MismatchedTypeIfCondition {
            expected_ty,
            found_ty,
            found_expr: *found_expr,
        },
        UnifyPurpose::IfThenElseBranch {
            expected_expr,
            found_expr,
        } => InferenceError::MismatchedTypeElseBranch {
            then_branch_ty: expected_ty,
            then_branch: *expected_expr,
            else_branch_ty: found_ty,
            else_branch: *found_expr,
        },
        UnifyPurpose::IfThenOnlyBranch { found_expr } => {
            InferenceError::MismatchedTypeOnlyIfBranch {
                then_branch_ty: found_ty,
                then_branch: *found_expr,
            }
        }
        UnifyPurpose::ReturnValue {
            expected_signature,
            found_expr,
        } => InferenceError::MismatchedTypeReturnValue {
            found_ty,
            found_expr: *found_expr,
            expected_signature: expected_signature.clone(),
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

    pub fn unify(&mut self, a: &Monotype, b: &Monotype, purpose: &UnifyPurpose) {
        let a_rep = self.find(a);
        let b_rep = self.find(b);

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
            (Monotype::Variable(_), b_rep) => self.unify_var(&a_rep, b_rep),
            (a_rep, Monotype::Variable(_)) => self.unify_var(&b_rep, a_rep),
            (_, _) => {
                self.errors
                    .push(build_unify_error_from_unify_purpose(a_rep, b_rep, purpose));
            }
        }
    }

    fn unify_var(&mut self, type_var: &Monotype, term: &Monotype) {
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
