//! # Mutability & Rebinding
//!
//! ## 概要
//!
//! Nail は、変数の値が変更されることを防ぐために、変数のミュータビリティと再代入を制御することができます。
//! ミュータビリティは以下の状態を持ちます。
//! - 可変
//! - 不変
//!
//! ## 可変性と再代入
//!
//! 変数のミュータビリティは、`mut` キーワードを使って指定します。
//! `mut` キーワードを指定しない場合、変数は不変となります。
//! 可変な場合、以下の二つを可能にします。
//! - 変数自体を更新すること
//! - 再代入を可能にすること
//!
//! ```ignore
//! let mut x = 1;
//! x = 2; // OK
//! ```
//!
//! ```ignore
//! let x = 1;
//! x = 2; // Error
//! ```
//!
//! ## 関数パラメータの可変性(検討中)
//!
//! 関数の引数の場合も`mut`をつけることで可変性の許可となります。
//! 再バインディングは許可されません。再バインディングしたい場合は`let mut`を使用します。
//!
//! ```ignore
//! fn foo(bar: i32) {
//!   bar = 2; // Error
//! }
//! ```
//!
//! ```ignore
//! fn foo(bar: mut i32) {
//!   bar = 2; // Error
//! }
//! ```
//!
//! ```ignore
//! fn foo(bar: mut i32) {
//!   let mut bar = bar;
//!   bar = 2; // Ok
//! }
//! ```

use std::collections::HashMap;

use crate::TypeCheckError;

pub(crate) struct FunctionMutabilityChecker<'a> {
    db: &'a dyn hir::HirMasterDatabase,
    pod: &'a hir::Pod,
    hir_file: hir::HirFile,
    function: hir::Function,

    binding_by_expr: HashMap<hir::ExprId, hir::BindingId>,
    errors: Vec<TypeCheckError>,
}
impl<'a> FunctionMutabilityChecker<'a> {
    pub(crate) fn new(
        db: &'a dyn hir::HirMasterDatabase,
        pod: &'a hir::Pod,
        hir_file: hir::HirFile,
        function: hir::Function,
    ) -> Self {
        Self {
            db,
            pod,
            hir_file,
            function,
            binding_by_expr: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub(crate) fn check(mut self) -> Vec<TypeCheckError> {
        let hir_file = self.pod.get_hir_file_by_function(self.function).unwrap();
        let hir::Expr::Block(body) = self.function.body(self.db, *hir_file).unwrap() else {
            panic!("Function body is not block");
        };

        for stmt in &body.stmts {
            self.check_stmt(stmt);
        }
        if let Some(tail) = body.tail {
            self.check_expr(tail);
        }

        self.errors
    }

    fn check_stmt(&mut self, stmt: &hir::Stmt) {
        match stmt {
            hir::Stmt::Expr {
                expr,
                has_semicolon: _,
            } => self.check_expr(*expr),
            hir::Stmt::Let { value, binding, .. } => {
                self.binding_by_expr.insert(*value, *binding);
                self.check_expr(*value);
            }
            hir::Stmt::Item { item } => match item {
                hir::Item::Function(_) => (),
                hir::Item::Module(_) => (),
                hir::Item::UseItem(_) => (),
            },
        }
    }

    fn check_expr(&mut self, expr_id: hir::ExprId) {
        let expr = expr_id.lookup(self.hir_file.db(self.db));
        match expr {
            hir::Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.check_stmt(stmt);
                }
                if let Some(tail) = block.tail {
                    self.check_expr(tail);
                }
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
            hir::Expr::Symbol(symbol) => match symbol {
                hir::Symbol::Param { .. } => (),
                // `VariableDef`で処理済みのため、ここでは何もしません
                hir::Symbol::Local { .. } => (),
                hir::Symbol::Missing { .. } => (),
            },
            hir::Expr::Binary { op, lhs, rhs } => {
                self.check_expr(*lhs);
                self.check_expr(*rhs);

                let lhs = lhs.lookup(self.hir_file.db(self.db));
                if let hir::Expr::Symbol(hir::Symbol::Local { binding, .. }) = lhs {
                    let binding = binding.lookup(self.hir_file.db(self.db));
                    if *op == hir::BinaryOp::Assign && !binding.mutable {
                        self.errors
                            .push(TypeCheckError::ImmutableReassignment { expr: expr_id });
                    }
                }
            }
            hir::Expr::Literal(_) => (),
            hir::Expr::Unary { op: _, expr } => {
                self.check_expr(*expr);
            }
            hir::Expr::Call { callee, args } => {
                for arg in args {
                    self.check_expr(*arg);
                }

                match callee {
                    hir::Symbol::Local { .. } => (),
                    hir::Symbol::Param { .. } => (),
                    hir::Symbol::Missing { .. } => (),
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
            hir::Expr::Missing => (),
        }
    }
}
