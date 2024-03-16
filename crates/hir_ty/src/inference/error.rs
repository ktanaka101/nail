use super::{Monotype, Signature};

/// 型チェックのエラー
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InferenceError {
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
    /// 関数の戻り値の型と`return`に指定した式の型が異なる
    MismatchedTypeReturnExpr {
        /// 期待される型
        expected_signature: Signature,
        /// 実際の型
        found_ty: Monotype,
        /// 実際のreturnの値
        found_return_expr: Option<hir::ExprId>,
        /// 実際のreturn式
        found_return: hir::ExprId,
    },
    /// 関数の戻り値の型と関数ブロックの最後の式の型が異なる
    MismatchedTypeReturnValue {
        /// 期待される型
        expected_signature: Signature,
        /// 期待される関数
        expected_function: hir::Function,
        /// ブロック最後の式
        found_ty: Monotype,
        /// ブロック最後の式
        found_last_expr: Option<hir::ExprId>,
    },
    /// 呼び出しの引数の数が一致しない
    MismatchedCallArgCount {
        /// 期待される引数の数
        expected_callee_arg_count: usize,
        /// 実際の引数の数
        found_arg_count: usize,
        /// 実際の呼び出し式
        found_expr: hir::ExprId,
    },
    /// 呼び出そうとしている対象が関数ではない
    NotCallable {
        /// 呼び出し対象の型
        found_callee_ty: Monotype,
        /// 呼び出し対象のシンボル
        found_callee_symbol: hir::Symbol,
        /// 呼び出し対象の式
        found_callee_expr: hir::ExprId,
    },
    /// モジュールが式として型推論されようとしている
    ModuleAsExpr {
        /// 実際のモジュール
        found_module: hir::Module,
        /// 実際の式
        found_expr: hir::ExprId,
    },
    /// 型が一致しない
    MismatchedType {
        /// 期待される型
        expected_ty: Monotype,
        /// 期待される型の式
        expected_expr: Option<hir::ExprId>,
        /// 実際の型
        found_ty: Monotype,
        /// 実際の式
        found_expr: hir::ExprId,
    },
    /// ループ外でbreak/continueが使われている
    BreakOutsideOfLoop {
        /// break/continue種類
        kind: BreakKind,
        /// 実際の式
        found_expr: hir::ExprId,
    },
}

/// ループ中断の種類
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BreakKind {
    /// continue
    Continue,
    /// break
    Break,
}
