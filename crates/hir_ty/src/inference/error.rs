use super::{Monotype, Signature};

/// 型チェックのエラー
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InferenceError {
    /// 一致するべき型が一致しない
    MismatchedTypes {
        /// 期待される型
        expected_ty: Monotype,
        /// 実際の型
        found_ty: Monotype,

        /// 期待される式
        expected_expr: hir::ExprId,
        /// 実際の式
        found_expr: hir::ExprId,
    },
    /// Ifの条件式の型が一致しない
    MismatchedTypeIfCondition {
        /// 期待される型
        expected_ty: Monotype,
        /// 実際の式
        found_expr: hir::ExprId,
        /// 実際の型
        found_ty: Monotype,
    },
    /// Ifのthenブランチとelseブランチの型が一致しない
    MismatchedTypeElseBranch {
        /// ifブランチの型
        then_branch_ty: Monotype,
        /// ifブランチの式
        then_branch: hir::ExprId,
        /// elseブランチの型
        else_branch_ty: Monotype,
        /// elseブランチの式
        else_branch: hir::ExprId,
    },
    /// Ifのthenブランチのみの型が一致しない
    MismatchedTypeOnlyIfBranch {
        /// thenブランチの型
        then_branch_ty: Monotype,
        /// thenブランチの式
        then_branch: hir::ExprId,
    },
    /// 関数呼び出しの引数の数が一致しない
    MismaatchedSignature {
        /// 期待される型
        expected_ty: Monotype,
        /// 呼び出そうとしている関数のシグネチャ
        signature: Signature,
        /// 実際の式
        found_expr: hir::ExprId,
        /// 実際の型
        found_ty: Monotype,
    },
    MismatchedBinaryInteger {
        /// 期待される型
        expected_ty: Monotype,
        /// 実際の式
        found_expr: hir::ExprId,
        /// 実際の型
        found_ty: Monotype,
        /// 演算子
        op: ast::BinaryOp,
    },
    MismatchedBinaryCompare {
        /// 期待される型
        expected_ty: Monotype,
        /// 期待される型を持つ式
        expected_expr: hir::ExprId,
        /// 実際の式
        found_expr: hir::ExprId,
        /// 実際の型
        found_ty: Monotype,
        /// 演算子
        op: ast::BinaryOp,
    },
    MismatchedUnary {
        /// 期待される型
        expected_ty: Monotype,
        /// 実際の式
        found_expr: hir::ExprId,
        /// 実際の型
        found_ty: Monotype,
        /// 演算子
        op: ast::UnaryOp,
    },
    /// 関数の戻り値の型と実際の戻り値の型が異なる
    ///
    /// 以下のいずれかが関数の戻り値の型と一致しない場合に発生する
    /// - `return`に指定した式の型
    /// - 関数ボディの最後の式の型
    MismatchedTypeReturnValue {
        /// 期待される型
        expected_signature: Signature,
        /// 実際の型
        found_ty: Monotype,
        /// 実際の式
        found_expr: Option<hir::ExprId>,
    },
}
