use super::{Monotype, Signature};

/// 型チェックのエラー
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InferenceError {
    /// 一致するべき型が一致しない
    MismatchedTypes {
        /// 期待される型
        expected_ty: Monotype,
        /// 期待される式
        expected_expr: hir::ExprId,
        /// 実際の型
        found_ty: Monotype,
        /// 実際の式
        found_expr: hir::ExprId,
    },
    /// Ifの条件式の型が一致しない
    MismatchedTypeIfCondition {
        /// 期待される型
        expected_condition_bool_ty: Monotype,
        /// 実際の型
        found_condition_ty: Monotype,
        /// 実際の式
        found_condition_expr: hir::ExprId,
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
        /// elseブランチの型
        else_branch_unit_ty: Monotype,
    },
    /// 関数呼び出しの型が一致しない
    MismaatchedTypeCallArg {
        /// 期待される引数の型
        expected_ty: Monotype,
        /// 実際の引数の型
        found_ty: Monotype,
        /// 期待される引数を持つ関数シグネチャ
        expected_signature: Signature,
        /// 実際の引数式
        found_expr: hir::ExprId,
        /// 実際の引数の位置(0-indexed)
        arg_pos: usize,
    },
    /// 数値をとる二項演算子の型が一致しない
    MismatchedBinaryInteger {
        /// 期待される型
        expected_int_ty: Monotype,
        /// 実際の式
        found_expr: hir::ExprId,
        /// 実際の型
        found_ty: Monotype,
        /// 演算子
        op: ast::BinaryOp,
    },
    /// 比較演算子の型が一致しない
    MismatchedBinaryCompare {
        /// 期待される型
        compare_from_ty: Monotype,
        /// 期待される型を持つ式
        compare_from_expr: hir::ExprId,
        /// 実際の型
        compare_to_ty: Monotype,
        /// 実際の式
        compare_to_expr: hir::ExprId,
        /// 演算子
        op: ast::BinaryOp,
    },
    /// 単項演算子の型が一致しない
    MismatchedUnary {
        /// 期待される型
        expected_ty: Monotype,
        /// 実際の型
        found_ty: Monotype,
        /// 実際の式
        found_expr: hir::ExprId,
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
        found_return_expr: Option<hir::ExprId>,
    },
    /// 呼び出しの引数の数が一致しない
    MismatchedCallArgCount {
        /// 期待される引数の数
        expected_callee_arg_count: usize,
        /// 実際の引数の数
        found_arg_count: usize,
    },
    /// 呼び出そうとしている対象が関数ではない
    NotCallable {
        /// 呼び出し対象の型
        found_callee_ty: Monotype,
        /// 呼び出し対象のシンボル
        found_callee_symbol: hir::Symbol,
    },
}
