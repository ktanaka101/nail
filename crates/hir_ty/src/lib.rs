//! HIRに型付けを行います。型推論はHindley-Milner型推論ベースで行います。
//! Typed HIRと呼びます。
//!
//! 以下のステップで行います。
//! 1. 型推論
//! 2. 型チェック
//!
//! 以下のように、HIRとTypedHIRはセットで扱います。TypedHIR単体では機能しません。
//! AST -----> HIR -------------------------------> MIR -----> LLVM IR
//!                \-----> TypedHIR(このcrate) ---/
//!

#![feature(trait_upcasting)]
// #[salsa::tracked]で生成される関数にドキュメントコメントが作成されないため警告が出てしまうため許可します。
#![allow(missing_docs)]

mod checker;
mod db;
mod inference;
mod testing;

pub use checker::{TypeCheckError, TypeCheckResult};
pub use db::{HirTyMasterDatabase, Jar};
pub use inference::{
    BreakKind, InferenceBodyResult, InferenceError, InferenceResult, Monotype, Signature,
};
pub use testing::TestingDatabase;

/// HIRを元にTypedHIRを構築します。
pub fn lower_pods(db: &dyn HirTyMasterDatabase, pods: &hir::Pods) -> TyLowerResult {
    let inference_result = inference::infer_pods(db, pods);
    let type_check_result = checker::check_type_pods(db, pods, &inference_result);

    TyLowerResult {
        inference_result,
        type_check_result,
    }
}

/// TypedHIRの構築結果です。
#[derive(Debug)]
pub struct TyLowerResult {
    /// 型推論の結果
    inference_result: InferenceResult,
    /// 型チェックの結果
    type_check_result: TypeCheckResult,
}
impl TyLowerResult {
    /// 指定した関数の型を取得します。
    pub fn signature_by_function(&self, function_id: hir::Function) -> Option<Signature> {
        self.inference_result
            .signature_by_function
            .get(&function_id)
            .copied()
    }

    /// 指定した関数の型推論結果を取得します。
    pub fn inference_body_by_function(
        &self,
        function: hir::Function,
    ) -> Option<&InferenceBodyResult> {
        self.inference_result
            .inference_body_result_by_function
            .get(&function)
    }

    /// 指定した関数の型チェック結果を取得します。
    pub fn type_check_errors_by_function(
        &self,
        function: hir::Function,
    ) -> Option<&Vec<TypeCheckError>> {
        self.type_check_result.errors_by_function.get(&function)
    }

    /// 型推論エラーを取得します。
    pub fn type_inference_errors_with_function(&self) -> Vec<(hir::Function, InferenceError)> {
        self.inference_result
            .inference_body_result_by_function
            .iter()
            .flat_map(|(function, inference_body_result)| {
                inference_body_result
                    .errors
                    .iter()
                    .map(|error| (*function, error.clone()))
            })
            .collect()
    }

    /// 型チェックエラーを取得します。
    pub fn type_check_errors(&self) -> Vec<TypeCheckError> {
        self.type_check_result
            .errors_by_function
            .values()
            .flatten()
            .cloned()
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{
        inference::{InferenceError, Monotype, Signature},
        lower_pods,
        testing::TestingDatabase,
        BreakKind, HirTyMasterDatabase, TyLowerResult, TypeCheckError,
    };

    fn check_pod_start_with_root_file(fixture: &str, expect: Expect) {
        let db = TestingDatabase::default();
        let source_db = hir::FixtureDatabase::new(&db, fixture);

        let pods = hir::parse_pods(&db, &source_db);
        let ty_lower_result = lower_pods(&db, &pods);

        expect.assert_eq(&TestingDebug::new(&db, &pods, &ty_lower_result).debug());
    }

    fn check_in_root_file(fixture: &str, expect: Expect) {
        let mut fixture = fixture.to_string();
        fixture.insert_str(0, "//- /main.nail\n");

        check_pod_start_with_root_file(&fixture, expect);
    }

    fn indent(nesting: usize) -> String {
        "    ".repeat(nesting)
    }

    struct TestingDebug<'a> {
        db: &'a dyn HirTyMasterDatabase,
        pods: &'a hir::Pods,
        ty_lower_result: &'a TyLowerResult,
    }
    impl<'a> TestingDebug<'a> {
        fn new(
            db: &'a dyn HirTyMasterDatabase,
            pods: &'a hir::Pods,
            ty_lower_result: &'a TyLowerResult,
        ) -> Self {
            Self {
                db,
                pods,
                ty_lower_result,
            }
        }

        fn debug(&self) -> String {
            let mut msg = "".to_string();

            msg.push_str(&self.debug_hir_file(self.pods.root_pod.root_hir_file));
            msg.push('\n');

            for (_nail_file, hir_file) in self.pods.root_pod.get_hir_files_order_registration_asc()
            {
                msg.push_str(&self.debug_hir_file(*hir_file));
                msg.push('\n');
            }

            msg.push_str("---\n");
            for (hir_file, function) in self.pods.root_pod.all_functions(self.db) {
                let inference_body_result = self
                    .ty_lower_result
                    .inference_result
                    .inference_body_result_by_function
                    .get(&function)
                    .unwrap();

                for error in &inference_body_result.errors {
                    match error {
                        InferenceError::MismatchedTypeIfCondition {
                            expected_condition_bool_ty,
                            found_condition_expr,
                            found_condition_ty,
                        } => {
                            msg.push_str(
                            &format!(
                                "error MismatchedTypeIfCondition: expected_ty: {}, found_ty: {}, found_expr: `{}`",
                                self.debug_monotype(expected_condition_bool_ty),
                                self.debug_monotype(found_condition_ty),
                                self.debug_simplify_expr(hir_file, *found_condition_expr),
                            ));
                        }
                        InferenceError::MismatchedTypeElseBranch {
                            then_branch_ty,
                            then_branch,
                            else_branch_ty,
                            else_branch,
                        } => {
                            msg.push_str(
                            &format!(
                                "error MismatchedTypeElseBranch: then_branch_ty: {}, else_branch_ty: {}, then_branch: `{}`, else_branch: `{}`",
                                self.debug_monotype(then_branch_ty),
                                self.debug_monotype(else_branch_ty),
                                self.debug_simplify_expr(hir_file, *then_branch),
                                self.debug_simplify_expr(hir_file, *else_branch),
                            ));
                        }
                        InferenceError::MismatchedTypeOnlyIfBranch {
                            then_branch_ty,
                            then_branch,
                            else_branch_unit_ty,
                        } => {
                            msg.push_str(
                            &format!(
                                "error MismatchedTypeOnlyIfBranch: then_branch_ty: {}, else_branch_ty: {}, then_branch: `{}`",
                                self.debug_monotype(then_branch_ty),
                                self.debug_monotype(else_branch_unit_ty),
                                self.debug_simplify_expr(hir_file, *then_branch),
                            ));
                        }
                        InferenceError::MismaatchedTypeCallArg {
                            expected_ty,
                            found_ty,
                            expected_signature,
                            found_expr,
                            arg_pos,
                        } => {
                            msg.push_str(
                            &format!(
                                "error MismaatchedSignature: expected_ty: {}, found_ty: {}, found_expr: `{}`, signature: {}, arg_pos: {}",
                                self.debug_monotype(expected_ty),
                                self.debug_monotype(found_ty),
                                self.debug_simplify_expr(hir_file, *found_expr),
                                self.debug_signature(expected_signature),
                                arg_pos
                            ));
                        }
                        InferenceError::MismatchedBinaryInteger {
                            expected_int_ty,
                            found_expr,
                            found_ty,
                            op,
                        } => {
                            msg.push_str(
                            &format!(
                                "error MismatchedBinaryInteger: op: {}, expected_ty: {}, found_ty: {}, found_expr: `{}`",
                                self.debug_binary_op(*op),
                                self.debug_monotype(expected_int_ty),
                                self.debug_monotype(found_ty),
                                self.debug_simplify_expr(hir_file, *found_expr),
                            ));
                        }
                        InferenceError::MismatchedBinaryCompare {
                            compare_from_ty,
                            compare_from_expr,
                            compare_to_expr,
                            compare_to_ty,
                            op,
                        } => {
                            msg.push_str(
                            &format!(
                                "error MismatchedBinaryCompare: op: {}, expected_ty: {}, found_ty: {}, expected_expr: `{}`, found_expr: `{}`",
                                self.debug_binary_op(*op),
                                self.debug_monotype(compare_from_ty),
                                self.debug_monotype(compare_to_ty),
                                self.debug_simplify_expr(hir_file, *compare_from_expr),
                                self.debug_simplify_expr(hir_file, *compare_to_expr),
                            ));
                        }
                        InferenceError::MismatchedUnary {
                            expected_ty,
                            found_expr,
                            found_ty,
                            op,
                        } => {
                            msg.push_str(
                            &format!(
                                "error MismatchedUnary: op: {}, expected_ty: {}, found_ty: {}, found_expr: `{}`",
                                self.debug_unary_op(*op),
                                self.debug_monotype(expected_ty),
                                self.debug_monotype(found_ty),
                                self.debug_simplify_expr(hir_file, *found_expr),
                            ));
                        }
                        InferenceError::MismatchedTypeReturnExpr {
                            expected_signature,
                            found_ty,
                            found_return_expr,
                            found_return: _,
                        } => {
                            if let Some(found_return_expr) = found_return_expr {
                                msg.push_str(
                            &format!(
                                "error MismatchedReturnType: expected_ty: {}, found_ty: {}, found_expr: `{}`",
                                self.debug_monotype(&expected_signature.return_type(self.db)),
                                self.debug_monotype(found_ty),
                                self.debug_simplify_expr(hir_file, *found_return_expr),
                            ));
                            } else {
                                msg.push_str(&format!(
                                    "error MismatchedReturnType: expected_ty: {}, found_ty: {}",
                                    self.debug_monotype(&expected_signature.return_type(self.db)),
                                    self.debug_monotype(found_ty),
                                ));
                            }
                        }
                        InferenceError::MismatchedTypeReturnValue {
                            expected_signature,
                            expected_function: _,
                            found_ty,
                            found_last_expr,
                        } => {
                            if let Some(found_last_expr) = found_last_expr {
                                msg.push_str(
                            &format!(
                                "error MismatchedTypeReturnValue: expected_ty: {}, found_ty: {}, found_expr: `{}`",
                                self.debug_monotype(&expected_signature.return_type(self.db)),
                                self.debug_monotype(found_ty),
                                self.debug_simplify_expr(hir_file, *found_last_expr),
                            ));
                            } else {
                                msg.push_str(&format!(
                                    "error MismatchedTypeReturnValue: expected_ty: {}, found_ty: {}",
                                    self.debug_monotype(&expected_signature.return_type(self.db)),
                                    self.debug_monotype(found_ty),
                                ));
                            }
                        }
                        InferenceError::MismatchedCallArgCount {
                            expected_callee_arg_count,
                            found_arg_count,
                            found_expr: _,
                        } => {
                            msg.push_str(&format!(
                                "error MismatchedCallArgCount: expected_arg_count: {}, found_arg_count: {}",
                                expected_callee_arg_count,
                                found_arg_count,
                            ));
                        }
                        InferenceError::NotCallable {
                            found_callee_ty,
                            found_callee_symbol,
                            found_callee_expr: _,
                        } => {
                            msg.push_str(&format!(
                                "error NotCallable: found_callee_ty: {}, found_callee_symbol: {}",
                                self.debug_monotype(found_callee_ty),
                                self.debug_symbol(found_callee_symbol),
                            ));
                        }
                        InferenceError::ModuleAsExpr {
                            found_module,
                            found_expr: _,
                        } => {
                            msg.push_str(&format!(
                                "error ModuleAsExpr: found_module: {}",
                                found_module.name(self.db).text(self.db)
                            ));
                        }
                        InferenceError::MismatchedType {
                            expected_ty,
                            expected_expr,
                            found_ty,
                            found_expr,
                        } => {
                            if let Some(expected_expr) = expected_expr {
                                msg.push_str(
                                &format!(
                                    "error MismatchedType: expected_ty: {}, found_ty: {}, expected_expr: {}, found_expr: `{}`",
                                    self.debug_monotype(expected_ty),
                                    self.debug_monotype(found_ty),
                                    self.debug_simplify_expr(hir_file, *expected_expr),
                                    self.debug_simplify_expr(hir_file, *found_expr),
                                ));
                            } else {
                                msg.push_str(
                                &format!(
                                "error MismatchedType: expected_ty: {}, found_ty: {}, found_expr: `{}`",
                                self.debug_monotype(expected_ty),
                                self.debug_monotype(found_ty),
                                self.debug_simplify_expr(hir_file, *found_expr),
                            ));
                            }
                        }
                        InferenceError::BreakOutsideOfLoop { kind, found_expr } => {
                            let kind_text = match kind {
                                BreakKind::Break => "break",
                                BreakKind::Continue => "continue",
                            };
                            msg.push_str(&format!(
                                "error BreakOutsideOfLoop({kind_text}): found_expr: `{}`",
                                self.debug_simplify_expr(hir_file, *found_expr),
                            ));
                        }
                    }
                    msg.push('\n');
                }
            }

            msg.push_str("---\n");
            for (hir_file, function) in self.pods.root_pod.all_functions(self.db) {
                let type_check_errors = self
                    .ty_lower_result
                    .type_check_errors_by_function(function)
                    .unwrap();
                for error in type_check_errors {
                    match error {
                        TypeCheckError::UnresolvedType { expr } => {
                            msg.push_str(&format!(
                                "error Type is unknown: expr: {}",
                                self.debug_simplify_expr(hir_file, *expr),
                            ));
                        }
                    }
                    msg.push('\n');
                }
            }

            msg
        }

        fn debug_signature(&self, signature: &Signature) -> String {
            let params = signature
                .params(self.db)
                .iter()
                .map(|param| self.debug_monotype(param))
                .collect::<Vec<_>>()
                .join(", ");
            let return_type = self.debug_monotype(&signature.return_type(self.db));

            format!("({params}) -> {return_type}")
        }

        fn debug_hir_file(&self, hir_file: hir::HirFile) -> String {
            let mut msg = format!(
                "//- {}\n",
                hir_file.file(self.db).file_path(self.db).to_str().unwrap()
            );

            for item in hir_file.top_level_items(self.db) {
                msg.push_str(&self.debug_item(hir_file, *item, 0));
            }

            msg
        }

        fn debug_function(
            &self,
            hir_file: hir::HirFile,
            function: hir::Function,
            nesting: usize,
        ) -> String {
            let body_expr = function.body(self.db, hir_file).unwrap();

            let name = function.name(self.db).text(self.db);
            let params = function
                .params(self.db)
                .iter()
                .map(|param| {
                    let name = if let Some(name) = param.data(hir_file.db(self.db)).name {
                        name.text(self.db)
                    } else {
                        "<missing>"
                    };
                    let ty = match param.data(hir_file.db(self.db)).ty {
                        hir::Type::Integer => "int",
                        hir::Type::String => "string",
                        hir::Type::Char => "char",
                        hir::Type::Boolean => "bool",
                        hir::Type::Unit => "()",
                        hir::Type::Unknown => "<unknown>",
                    };
                    format!("{name}: {ty}")
                })
                .collect::<Vec<_>>()
                .join(", ");
            let return_type = match &function.return_type(self.db) {
                hir::Type::Integer => "int",
                hir::Type::String => "string",
                hir::Type::Char => "char",
                hir::Type::Boolean => "bool",
                hir::Type::Unit => "()",
                hir::Type::Unknown => "<unknown>",
            };

            let hir::Expr::Block(block) = body_expr else {
                panic!("Should be Block")
            };

            let mut body = "{\n".to_string();
            for stmt in &block.stmts {
                body.push_str(&self.debug_stmt(hir_file, function, stmt, nesting + 1));
            }
            if let Some(tail) = block.tail {
                let indent = indent(nesting + 1);
                body.push_str(&format!(
                    "{indent}expr:{}",
                    self.debug_expr(hir_file, function, tail, nesting + 1)
                ));
                body.push_str(&format!(" {}\n", self.debug_type_line(function, tail)));
            }
            body.push_str(&format!("{}}}", indent(nesting)));

            let is_entry_point = hir_file.entry_point(self.db) == Some(function);
            format!(
                "{}fn {}{name}({params}) -> {return_type} {body}\n",
                indent(nesting),
                if is_entry_point { "entry:" } else { "" }
            )
        }

        fn debug_module(
            &self,
            hir_file: hir::HirFile,
            module: hir::Module,
            nesting: usize,
        ) -> String {
            let _scope_origin = hir::ModuleScopeOrigin::Module { origin: module };

            let curr_indent = indent(nesting);

            let module_name = module.name(self.db).text(self.db);

            match module.kind(self.db) {
                hir::ModuleKind::Inline { items } => {
                    let mut module_str = "".to_string();
                    module_str.push_str(&format!("{curr_indent}mod {module_name} {{\n"));
                    for (i, item) in items.iter().enumerate() {
                        module_str.push_str(&self.debug_item(hir_file, *item, nesting + 1));
                        if i == items.len() - 1 {
                            continue;
                        }

                        module_str.push('\n');
                    }
                    module_str.push_str(&format!("{curr_indent}}}\n"));
                    module_str
                }
                hir::ModuleKind::Outline => {
                    format!("{curr_indent}mod {module_name};\n")
                }
            }
        }

        fn debug_use_item(&self, use_item: hir::UseItem) -> String {
            let path_name = self.debug_path(use_item.path(self.db));
            let item_name = use_item.name(self.db).text(self.db);

            if path_name.is_empty() {
                format!("use {item_name};\n")
            } else {
                format!("use {path_name}::{item_name};\n")
            }
        }

        fn debug_item(&self, hir_file: hir::HirFile, item: hir::Item, nesting: usize) -> String {
            match item {
                hir::Item::Function(function) => self.debug_function(hir_file, function, nesting),
                hir::Item::Module(module) => self.debug_module(hir_file, module, nesting),
                hir::Item::UseItem(use_item) => self.debug_use_item(use_item),
            }
        }

        fn debug_stmt(
            &self,
            hir_file: hir::HirFile,
            function: hir::Function,
            stmt: &hir::Stmt,
            nesting: usize,
        ) -> String {
            match stmt {
                hir::Stmt::VariableDef { name, value } => {
                    let indent = indent(nesting);
                    let name = name.text(self.db);
                    let expr_str = self.debug_expr(hir_file, function, *value, nesting);
                    let mut stmt_str = format!("{indent}let {name} = {expr_str};");

                    let type_line = self.debug_type_line(function, *value);
                    stmt_str.push_str(&format!(" {type_line}\n"));

                    stmt_str
                }
                hir::Stmt::ExprStmt {
                    expr,
                    has_semicolon,
                } => {
                    let indent = indent(nesting);
                    let expr_str = self.debug_expr(hir_file, function, *expr, nesting);
                    let type_line = self.debug_type_line(function, *expr);
                    let maybe_semicolon = if *has_semicolon { ";" } else { "" };
                    format!("{indent}{expr_str}{maybe_semicolon} {type_line}\n")
                }
                hir::Stmt::Item { item } => self.debug_item(hir_file, *item, nesting),
            }
        }

        fn debug_type_line(&self, function: hir::Function, expr_id: hir::ExprId) -> String {
            let ty = self
                .ty_lower_result
                .inference_body_by_function(function)
                .unwrap()
                .type_by_expr
                .get(&expr_id)
                .unwrap();

            format!("//: {}", self.debug_monotype(ty))
        }

        fn debug_monotype(&self, monotype: &Monotype) -> String {
            match monotype {
                Monotype::Integer => "int".to_string(),
                Monotype::Bool => "bool".to_string(),
                Monotype::Unit => "()".to_string(),
                Monotype::Char => "char".to_string(),
                Monotype::String => "string".to_string(),
                Monotype::Variable(id) => format!("${}", id),
                Monotype::Function(signature) => self.debug_signature(signature),
                Monotype::Never => "!".to_string(),
                Monotype::Unknown => "<unknown>".to_string(),
            }
        }

        fn debug_expr(
            &self,
            hir_file: hir::HirFile,
            function: hir::Function,
            expr_id: hir::ExprId,
            nesting: usize,
        ) -> String {
            match expr_id.lookup(hir_file.db(self.db)) {
                hir::Expr::Symbol(symbol) => self.debug_symbol(symbol),
                hir::Expr::Literal(literal) => match literal {
                    hir::Literal::Bool(b) => b.to_string(),
                    hir::Literal::Char(c) => format!("'{c}'"),
                    hir::Literal::String(s) => format!("\"{s}\""),
                    hir::Literal::Integer(i) => i.to_string(),
                },
                hir::Expr::Binary { op, lhs, rhs } => {
                    let op = match op {
                        hir::BinaryOp::Add => "+",
                        hir::BinaryOp::Sub => "-",
                        hir::BinaryOp::Mul => "*",
                        hir::BinaryOp::Div => "/",
                        hir::BinaryOp::Equal => "==",
                        hir::BinaryOp::GreaterThan => ">",
                        hir::BinaryOp::LessThan => "<",
                        hir::BinaryOp::Assign => "=",
                    };
                    let lhs_str = self.debug_expr(hir_file, function, *lhs, nesting);
                    let rhs_str = self.debug_expr(hir_file, function, *rhs, nesting);
                    format!("{lhs_str} {op} {rhs_str}")
                }
                hir::Expr::Unary { op, expr } => {
                    let op = match op {
                        hir::UnaryOp::Neg => "-",
                        hir::UnaryOp::Not => "!",
                    };
                    let expr_str = self.debug_expr(hir_file, function, *expr, nesting);
                    format!("{op}{expr_str}")
                }
                hir::Expr::Call { callee, args } => {
                    let callee = self.debug_symbol(callee);
                    let args = args
                        .iter()
                        .map(|arg| self.debug_expr(hir_file, function, *arg, nesting))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{callee}({args})")
                }
                hir::Expr::Block(block) => {
                    let mut msg = "{\n".to_string();
                    for stmt in &block.stmts {
                        msg.push_str(&self.debug_stmt(hir_file, function, stmt, nesting + 1));
                    }
                    if let Some(tail) = block.tail {
                        let indent = indent(nesting + 1);
                        msg.push_str(&format!(
                            "{indent}expr:{}",
                            self.debug_expr(hir_file, function, tail, nesting + 1)
                        ));
                        msg.push_str(&format!(" {}\n", self.debug_type_line(function, tail)));
                    }
                    msg.push_str(&format!("{}}}", indent(nesting)));

                    msg
                }
                hir::Expr::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let mut msg = "if ".to_string();
                    msg.push_str(&self.debug_expr(hir_file, function, *condition, nesting));
                    msg.push(' ');
                    msg.push_str(&self.debug_expr(hir_file, function, *then_branch, nesting));

                    if let Some(else_branch) = else_branch {
                        msg.push_str(" else ");
                        msg.push_str(&self.debug_expr(hir_file, function, *else_branch, nesting));
                    }

                    msg
                }
                hir::Expr::Return { value } => {
                    let mut msg = "return".to_string();
                    if let Some(value) = value {
                        msg.push_str(&format!(
                            " {}",
                            &self.debug_expr(hir_file, function, *value, nesting,)
                        ));
                    }

                    msg
                }
                hir::Expr::Loop { block } => {
                    let mut msg = "loop ".to_string();
                    msg.push_str(&self.debug_expr(hir_file, function, *block, nesting));

                    msg
                }
                hir::Expr::Continue => "continue".to_string(),
                hir::Expr::Break { value } => {
                    let mut msg = "break".to_string();
                    if let Some(value) = value {
                        msg.push_str(&format!(
                            " {}",
                            &self.debug_expr(hir_file, function, *value, nesting,)
                        ));
                    }

                    msg
                }
                hir::Expr::Missing => "<missing>".to_string(),
            }
        }

        fn debug_simplify_expr(&self, hir_file: hir::HirFile, expr_id: hir::ExprId) -> String {
            match expr_id.lookup(hir_file.db(self.db)) {
                hir::Expr::Symbol(symbol) => self.debug_symbol(symbol),
                hir::Expr::Literal(literal) => match literal {
                    hir::Literal::Bool(b) => b.to_string(),
                    hir::Literal::Char(c) => format!("'{c}'"),
                    hir::Literal::String(s) => format!("\"{s}\""),
                    hir::Literal::Integer(i) => i.to_string(),
                },
                hir::Expr::Binary { op, lhs, rhs } => {
                    let op = match op {
                        hir::BinaryOp::Add => "+",
                        hir::BinaryOp::Sub => "-",
                        hir::BinaryOp::Mul => "*",
                        hir::BinaryOp::Div => "/",
                        hir::BinaryOp::Equal => "==",
                        hir::BinaryOp::GreaterThan => ">",
                        hir::BinaryOp::LessThan => "<",
                        hir::BinaryOp::Assign => "=",
                    };
                    let lhs_str = self.debug_simplify_expr(hir_file, *lhs);
                    let rhs_str = self.debug_simplify_expr(hir_file, *rhs);
                    format!("{lhs_str} {op} {rhs_str}")
                }
                hir::Expr::Unary { op, expr } => {
                    let op = match op {
                        hir::UnaryOp::Neg => "-",
                        hir::UnaryOp::Not => "!",
                    };
                    let expr_str = self.debug_simplify_expr(hir_file, *expr);
                    format!("{op}{expr_str}")
                }
                hir::Expr::Call { callee, args } => {
                    let callee = self.debug_symbol(callee);
                    let args = args
                        .iter()
                        .map(|arg| self.debug_simplify_expr(hir_file, *arg))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{callee}({args})")
                }
                hir::Expr::Block(block) => {
                    let mut msg = "{ tail:".to_string();
                    if let Some(tail) = block.tail {
                        msg.push_str(&self.debug_simplify_expr(hir_file, tail));
                    } else {
                        msg.push_str("none");
                    }
                    msg.push_str(" }");

                    msg
                }
                hir::Expr::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let cond_str = self.debug_simplify_expr(hir_file, *condition);
                    let mut msg = format!(
                        "if {cond_str} {}",
                        self.debug_simplify_expr(hir_file, *then_branch)
                    );

                    if let Some(else_branch) = else_branch {
                        msg.push_str(&format!(
                            " else {}",
                            self.debug_simplify_expr(hir_file, *else_branch)
                        ));
                    }

                    msg
                }
                hir::Expr::Return { value } => {
                    let mut msg = "return".to_string();
                    if let Some(value) = value {
                        msg.push_str(&format!(
                            " {}",
                            &self.debug_simplify_expr(hir_file, *value,)
                        ));
                    }

                    msg
                }
                hir::Expr::Loop { block } => {
                    let mut msg = "loop ".to_string();
                    msg.push_str(&self.debug_simplify_expr(hir_file, *block));

                    msg
                }
                hir::Expr::Continue => "continue".to_string(),
                hir::Expr::Break { value } => {
                    let mut msg = "break".to_string();
                    if let Some(value) = value {
                        msg.push_str(&format!(
                            " {}",
                            &self.debug_simplify_expr(hir_file, *value,)
                        ));
                    }

                    msg
                }
                hir::Expr::Missing => "<missing>".to_string(),
            }
        }

        fn debug_symbol(&self, symbol: &hir::Symbol) -> String {
            match &symbol {
                hir::Symbol::Local { name, expr: _ } => name.text(self.db).to_string(),
                hir::Symbol::Param { name, .. } => {
                    let name = name.text(self.db);
                    format!("param:{name}")
                }
                hir::Symbol::Missing { path } => {
                    let resolving_status = self.pods.resolution_map.item_by_symbol(path).unwrap();
                    self.debug_resolution_status(resolving_status)
                }
            }
        }

        fn debug_binary_op(&self, op: hir::BinaryOp) -> String {
            match op {
                hir::BinaryOp::Add => "+",
                hir::BinaryOp::Sub => "-",
                hir::BinaryOp::Mul => "*",
                hir::BinaryOp::Div => "/",
                hir::BinaryOp::Equal => "==",
                hir::BinaryOp::GreaterThan => ">",
                hir::BinaryOp::LessThan => "<",
                hir::BinaryOp::Assign => "=",
            }
            .to_string()
        }

        fn debug_unary_op(&self, op: hir::UnaryOp) -> String {
            match op {
                hir::UnaryOp::Neg => "-",
                hir::UnaryOp::Not => "!",
            }
            .to_string()
        }

        fn debug_resolution_status(&self, resolution_status: hir::ResolutionStatus) -> String {
            match resolution_status {
                hir::ResolutionStatus::Unresolved => "<unknown>".to_string(),
                hir::ResolutionStatus::Error => "<missing>".to_string(),
                hir::ResolutionStatus::Resolved { path, item } => {
                    let path = self.debug_path(&path);
                    match item {
                        hir::Item::Function(_) => {
                            format!("fn:{path}")
                        }
                        hir::Item::Module(_) => {
                            format!("mod:{path}")
                        }
                        hir::Item::UseItem(use_item) => {
                            let item = self
                                .pods
                                .resolution_map
                                .item_by_use_item(&use_item)
                                .unwrap();
                            format!(
                                "{}::{}",
                                self.debug_resolution_status(item),
                                use_item.name(self.db).text(self.db)
                            )
                        }
                    }
                }
            }
        }

        fn debug_path(&self, path: &hir::Path) -> String {
            path.segments(self.db)
                .iter()
                .map(|segment| segment.text(self.db).to_string())
                .collect::<Vec<_>>()
                .join("::")
        }
    }

    #[test]
    fn test_fibonacci() {
        check_in_root_file(
            r#"
            fn fibonacci(x: int) -> int {
                if x == 0 {
                    0
                } else {
                    if x == 1 {
                        1
                    } else {
                        fibonacci(x - 1) + fibonacci(x - 2)
                    }
                }
            }
            fn main() -> int {
                fibonacci(15)
            }
        "#,
            expect![[r#"
                //- /main.nail
                fn fibonacci(x: int) -> int {
                    expr:if param:x == 0 {
                        expr:0 //: int
                    } else {
                        expr:if param:x == 1 {
                            expr:1 //: int
                        } else {
                            expr:fn:fibonacci(param:x - 1) + fn:fibonacci(param:x - 2) //: int
                        } //: int
                    } //: int
                }
                fn entry:main() -> int {
                    expr:fn:fibonacci(15) //: int
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_integer_literal() {
        check_in_root_file(
            r#"
                fn main() {
                    10;
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    10; //: int
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_string_literal() {
        check_in_root_file(
            r#"
                fn main() {
                    "aaa";
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    "aaa"; //: string
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_char_literal() {
        check_in_root_file(
            r#"
                fn main() {
                    'a';
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    'a'; //: char
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_bool_test() {
        check_in_root_file(
            r#"
                fn main() {
                    true;
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    true; //: bool
                }

                ---
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() {
                    false;
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    false; //: bool
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_variable_def() {
        check_in_root_file(
            r#"
                fn main() {
                    let a = true
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    let a = true; //: bool
                }

                ---
                ---
            "#]],
        )
    }

    #[test]
    fn infer_multiline_variable_def() {
        check_in_root_file(
            r#"
                fn main() {
                    let a = true
                    let b = 10
                    let c = "aa"
                    let d = 'a'
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    let a = true; //: bool
                    let b = 10; //: int
                    let c = "aa"; //: string
                    let d = 'a'; //: char
                }

                ---
                ---
            "#]],
        )
    }

    #[test]
    fn infer_binary() {
        check_in_root_file(
            r#"
                fn main() {
                    10 + 20
                    "aaa" + "bbb"
                    10 + "aaa"
                    'a' + 'a'
                    10 + 'a'
                    10 < 'a'
                    10 > 'a'
                    true + true
                    true - true
                    true * true
                    true / true
                    true == true
                    true < false
                    true > false
                    10 + true
                    10 + (10 + "aaa")
                    10 - 20
                    10 * 20
                    10 / 20
                    10 == 20
                    10 < 20
                    10 > 20;
                    let a = 10;
                    a = 20;
                    a = "aaa";
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    10 + 20 //: int
                    "aaa" + "bbb" //: int
                    10 + "aaa" //: int
                    'a' + 'a' //: int
                    10 + 'a' //: int
                    10 < 'a' //: bool
                    10 > 'a' //: bool
                    true + true //: int
                    true - true //: int
                    true * true //: int
                    true / true //: int
                    true == true //: bool
                    true < false //: bool
                    true > false //: bool
                    10 + true //: int
                    10 + 10 + "aaa" //: int
                    10 - 20 //: int
                    10 * 20 //: int
                    10 / 20 //: int
                    10 == 20 //: bool
                    10 < 20 //: bool
                    10 > 20; //: bool
                    let a = 10; //: int
                    a = 20; //: ()
                    a = "aaa"; //: ()
                }

                ---
                error MismatchedBinaryInteger: op: +, expected_ty: int, found_ty: string, found_expr: `"aaa"`
                error MismatchedBinaryInteger: op: +, expected_ty: int, found_ty: string, found_expr: `"bbb"`
                error MismatchedBinaryInteger: op: +, expected_ty: int, found_ty: string, found_expr: `"aaa"`
                error MismatchedBinaryInteger: op: +, expected_ty: int, found_ty: char, found_expr: `'a'`
                error MismatchedBinaryInteger: op: +, expected_ty: int, found_ty: char, found_expr: `'a'`
                error MismatchedBinaryInteger: op: +, expected_ty: int, found_ty: char, found_expr: `'a'`
                error MismatchedBinaryCompare: op: <, expected_ty: int, found_ty: char, expected_expr: `10`, found_expr: `'a'`
                error MismatchedBinaryCompare: op: >, expected_ty: int, found_ty: char, expected_expr: `10`, found_expr: `'a'`
                error MismatchedBinaryInteger: op: +, expected_ty: int, found_ty: bool, found_expr: `true`
                error MismatchedBinaryInteger: op: +, expected_ty: int, found_ty: bool, found_expr: `true`
                error MismatchedBinaryInteger: op: -, expected_ty: int, found_ty: bool, found_expr: `true`
                error MismatchedBinaryInteger: op: -, expected_ty: int, found_ty: bool, found_expr: `true`
                error MismatchedBinaryInteger: op: *, expected_ty: int, found_ty: bool, found_expr: `true`
                error MismatchedBinaryInteger: op: *, expected_ty: int, found_ty: bool, found_expr: `true`
                error MismatchedBinaryInteger: op: /, expected_ty: int, found_ty: bool, found_expr: `true`
                error MismatchedBinaryInteger: op: /, expected_ty: int, found_ty: bool, found_expr: `true`
                error MismatchedBinaryInteger: op: +, expected_ty: int, found_ty: bool, found_expr: `true`
                error MismatchedBinaryInteger: op: +, expected_ty: int, found_ty: string, found_expr: `"aaa"`
                error MismatchedType: expected_ty: int, found_ty: string, expected_expr: a, found_expr: `"aaa"`
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() {
                    (10 + "aaa") + (10 + "aaa");
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    10 + "aaa" + 10 + "aaa"; //: int
                }

                ---
                error MismatchedBinaryInteger: op: +, expected_ty: int, found_ty: string, found_expr: `"aaa"`
                error MismatchedBinaryInteger: op: +, expected_ty: int, found_ty: string, found_expr: `"aaa"`
                ---
            "#]],
        );
    }

    #[test]
    fn infer_unary() {
        check_in_root_file(
            r#"
                fn main() {
                    let a = -10;
                    let b = -"aaa";
                    let c = -'a';
                    let d = -true;

                    let e = !10;
                    let f = !"aaa";
                    let g = !'a';
                    let h = !true;
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    let a = -10; //: int
                    let b = -"aaa"; //: int
                    let c = -'a'; //: int
                    let d = -true; //: int
                    let e = !10; //: bool
                    let f = !"aaa"; //: bool
                    let g = !'a'; //: bool
                    let h = !true; //: bool
                }

                ---
                error MismatchedUnary: op: -, expected_ty: int, found_ty: string, found_expr: `"aaa"`
                error MismatchedUnary: op: -, expected_ty: int, found_ty: char, found_expr: `'a'`
                error MismatchedUnary: op: -, expected_ty: int, found_ty: bool, found_expr: `true`
                error MismatchedUnary: op: !, expected_ty: bool, found_ty: int, found_expr: `10`
                error MismatchedUnary: op: !, expected_ty: bool, found_ty: string, found_expr: `"aaa"`
                error MismatchedUnary: op: !, expected_ty: bool, found_ty: char, found_expr: `'a'`
                ---
            "#]],
        )
    }

    #[test]
    fn aaa() {
        check_in_root_file(
            r#"
                fn main() -> bool {
                    let a = !true;
                    let b = !false;
                    !a == !b
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> bool {
                    let a = !true; //: bool
                    let b = !false; //: bool
                    expr:!a == !b //: bool
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_variable_ref() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    let a = -10
                    a
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    let a = -10; //: int
                    expr:a //: int
                }

                ---
                ---
            "#]],
        )
    }

    #[test]
    fn infer_block() {
        check_in_root_file(
            r#"
                fn main() {
                    {
                        10
                    };
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    {
                        expr:10 //: int
                    }; //: int
                }

                ---
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() {
                    {
                        {
                            10
                            "aaa"
                        }
                    };
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    {
                        expr:{
                            10 //: int
                            expr:"aaa" //: string
                        } //: string
                    }; //: string
                }

                ---
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    let a = 10;
                    let b = {
                        let c = 20;
                        a + c
                    }
                    b
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    let a = 10; //: int
                    let b = {
                        let c = 20; //: int
                        expr:a + c //: int
                    }; //: int
                    expr:b //: int
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_last_expr_stmt_with_semicolon_only_as_expr() {
        check_in_root_file(
            r#"
                fn aaa() {
                    10
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn aaa() -> () {
                    expr:10 //: int
                }

                ---
                error MismatchedTypeReturnValue: expected_ty: (), found_ty: int, found_expr: `10`
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn aaa() {
                    10;
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn aaa() -> () {
                    10; //: int
                }

                ---
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn aaa() {
                    {
                        10
                    };
                    {
                        20;
                    };
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn aaa() -> () {
                    {
                        expr:10 //: int
                    }; //: int
                    {
                        20; //: int
                    }; //: ()
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_nesting_last_expr_stmt_with_semicolon_only_as_expr() {
        check_in_root_file(
            r#"
                fn aaa() {
                    {
                        {
                            10
                        }
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn aaa() -> () {
                    expr:{
                        expr:{
                            expr:10 //: int
                        } //: int
                    } //: int
                }

                ---
                error MismatchedTypeReturnValue: expected_ty: (), found_ty: int, found_expr: `{ tail:{ tail:10 } }`
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn aaa() {
                    {
                        {
                            10;
                        }
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn aaa() -> () {
                    expr:{
                        expr:{
                            10; //: int
                        } //: ()
                    } //: ()
                }

                ---
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn aaa() {
                    {
                        {
                            10
                        };
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn aaa() -> () {
                    expr:{
                        {
                            expr:10 //: int
                        }; //: int
                    } //: ()
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_function() {
        check_in_root_file(
            r#"
                fn aaa() -> int {
                    let a = 10
                    a
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn aaa() -> int {
                    let a = 10; //: int
                    expr:a //: int
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_function_param() {
        check_in_root_file(
            r#"
                fn aaa(x: int, y: string) {
                    let a = x
                    let b = y
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn aaa(x: int, y: string) -> () {
                    let a = param:x; //: int
                    let b = param:y; //: string
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_call() {
        check_in_root_file(
            r#"
                fn main() {
                    fn aaa(x: bool, y: string) -> int {
                        10 + 20
                    }
                    let res = aaa(true, "aaa");
                    res + 30
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    fn aaa(x: bool, y: string) -> int {
                        expr:10 + 20 //: int
                    }
                    let res = fn:aaa(true, "aaa"); //: int
                    expr:res + 30 //: int
                }

                ---
                error MismatchedTypeReturnValue: expected_ty: (), found_ty: int, found_expr: `res + 30`
                ---
            "#]],
        );
    }

    #[test]
    fn infer_call_missmatch() {
        check_in_root_file(
            r#"
                fn main() {
                    fn aaa(x: bool, y: string) -> int {
                        10 + 20
                    }
                    aaa("aaa", true);
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    fn aaa(x: bool, y: string) -> int {
                        expr:10 + 20 //: int
                    }
                    fn:aaa("aaa", true); //: int
                }

                ---
                error MismaatchedSignature: expected_ty: bool, found_ty: string, found_expr: `"aaa"`, signature: (bool, string) -> int, arg_pos: 0
                error MismaatchedSignature: expected_ty: string, found_ty: bool, found_expr: `true`, signature: (bool, string) -> int, arg_pos: 1
                ---
            "#]],
        );
    }

    #[test]
    fn infer_if_expr() {
        check_in_root_file(
            r#"
                fn main() {
                    if true {
                        10
                    } else {
                        20
                    };
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    if true {
                        expr:10 //: int
                    } else {
                        expr:20 //: int
                    }; //: int
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_if_expr_else_is_unit() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        10
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    expr:if true {
                        expr:10 //: int
                    } //: int
                }

                ---
                error MismatchedTypeOnlyIfBranch: then_branch_ty: int, else_branch_ty: (), then_branch: `{ tail:10 }`
                ---
            "#]],
        );
    }

    #[test]
    fn infer_if_expr_empty_block_is_unit() {
        check_in_root_file(
            r#"
                fn main() {
                    if true {
                    } else {
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    expr:if true {
                    } else {
                    } //: ()
                }

                ---
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() {
                    if true {
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    expr:if true {
                    } //: ()
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_if_expr_mismatched_type() {
        check_in_root_file(
            r#"
                fn main() {
                    if true {
                        10
                    } else {
                        "aaa"
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    expr:if true {
                        expr:10 //: int
                    } else {
                        expr:"aaa" //: string
                    } //: int
                }

                ---
                error MismatchedTypeElseBranch: then_branch_ty: int, else_branch_ty: string, then_branch: `{ tail:10 }`, else_branch: `{ tail:"aaa" }`
                error MismatchedTypeReturnValue: expected_ty: (), found_ty: int, found_expr: `if true { tail:10 } else { tail:"aaa" }`
                ---
            "#]],
        );
    }

    #[test]
    fn infer_if_expr_condition_is_not_bool() {
        check_in_root_file(
            r#"
                fn main() {
                    if 10 {
                        "aaa"
                    } else {
                        "aaa"
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    expr:if 10 {
                        expr:"aaa" //: string
                    } else {
                        expr:"aaa" //: string
                    } //: string
                }

                ---
                error MismatchedTypeIfCondition: expected_ty: bool, found_ty: int, found_expr: `10`
                error MismatchedTypeReturnValue: expected_ty: (), found_ty: string, found_expr: `if 10 { tail:"aaa" } else { tail:"aaa" }`
                ---
            "#]],
        );
    }

    #[test]
    fn infer_return_in_if_expr() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    let value =
                        if true {
                            return 10;
                        } else {
                            true
                        };

                    20
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    let value = if true {
                        return 10; //: !
                    } else {
                        expr:true //: bool
                    }; //: bool
                    expr:20 //: int
                }

                ---
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        return 10;
                    } else {
                        20
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    expr:if true {
                        return 10; //: !
                    } else {
                        expr:20 //: int
                    } //: int
                }

                ---
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    let value =
                        if true {
                            true
                        } else {
                            return 10;
                        };

                    20
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    let value = if true {
                        expr:true //: bool
                    } else {
                        return 10; //: !
                    }; //: bool
                    expr:20 //: int
                }

                ---
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    let value =
                        if true {
                            return 10;
                        } else {
                            return 20;
                        };

                    30
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    let value = if true {
                        return 10; //: !
                    } else {
                        return 20; //: !
                    }; //: !
                    expr:30 //: int
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_return_in_function() {
        check_in_root_file(
            r#"
                fn main() {
                    return
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    expr:return //: !
                }

                ---
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    return 10
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    expr:return 10 //: !
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_return_in_function_missing_types() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    return
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    expr:return //: !
                }

                ---
                error MismatchedReturnType: expected_ty: int, found_ty: ()
                ---
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    return "aaa"
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    expr:return "aaa" //: !
                }

                ---
                error MismatchedReturnType: expected_ty: int, found_ty: string, found_expr: `"aaa"`
                ---
            "#]],
        );
    }

    #[test]
    fn infer_loop() {
        check_in_root_file(
            r#"
                fn main() {
                    loop {
                        10;
                        20;
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    expr:loop {
                        10; //: int
                        20; //: int
                    } //: !
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_break_no_expr() {
        check_in_root_file(
            r#"
                fn main() {
                    loop {
                        break;
                        break;
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    expr:loop {
                        break; //: !
                        break; //: !
                    } //: ()
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_break_expr() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        break 10;
                        break 20;
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    expr:loop {
                        break 10; //: !
                        break 20; //: !
                    } //: int
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_break_expr_mismatched_type() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        break 10;
                        break "aaa";
                        break;
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    expr:loop {
                        break 10; //: !
                        break "aaa"; //: !
                        break; //: !
                    } //: int
                }

                ---
                error MismatchedType: expected_ty: int, found_ty: string, found_expr: `break "aaa"`
                error MismatchedType: expected_ty: int, found_ty: (), found_expr: `break`
                ---
            "#]],
        );
    }

    #[test]
    fn infer_break_in_nested_loop() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        break 10;

                        loop {
                            break "aaa";

                            loop {
                                break;
                            }

                            break "bbb";
                        }

                        break 20;

                        fn f1() -> bool {
                            loop {
                                break true;
                            }
                        }
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    expr:loop {
                        break 10; //: !
                        loop {
                            break "aaa"; //: !
                            loop {
                                break; //: !
                            } //: ()
                            break "bbb"; //: !
                        } //: string
                        break 20; //: !
                        fn f1() -> bool {
                            expr:loop {
                                break true; //: !
                            } //: bool
                        }
                    } //: int
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn break_outside_of_loop() {
        check_in_root_file(
            r#"
                fn main() {
                    loop {
                    }

                    break;
                    break 10;
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    loop {
                    } //: !
                    break; //: !
                    break 10; //: !
                }

                ---
                error BreakOutsideOfLoop(break): found_expr: `break`
                error BreakOutsideOfLoop(break): found_expr: `break 10`
                ---
            "#]],
        );
    }

    #[test]
    fn continue_in_loop() {
        check_in_root_file(
            r#"
                fn main() {
                    loop {
                        continue;
                        continue;
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    expr:loop {
                        continue; //: !
                        continue; //: !
                    } //: !
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn continue_outside_of_loop() {
        check_in_root_file(
            r#"
                fn main() {
                    loop {
                    }

                    continue;
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    loop {
                    } //: !
                    continue; //: !
                }

                ---
                error BreakOutsideOfLoop(continue): found_expr: `continue`
                ---
            "#]],
        );
    }

    #[test]
    fn loop_scope() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    let a = 0;
                    loop {
                        let b = 1;
                        a
                        b
                        break;
                    }

                    a
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    let a = 0; //: int
                    loop {
                        let b = 1; //: int
                        a //: int
                        b //: int
                        break; //: !
                    } //: ()
                    expr:a //: int
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_while() {
        check_in_root_file(
            r#"
                fn main() -> int {
                    let i = 1;
                    while i < 3 {
                        i = i + 1;
                    }

                    i
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    let i = 1; //: int
                    loop {
                        if i < 3 {
                            i = i + 1; //: ()
                        } else {
                            break; //: !
                        }; //: ()
                    } //: ()
                    expr:i //: int
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn infer_modules() {
        check_in_root_file(
            r#"
                fn main() {
                    return;
                }
                mod module_aaa {
                    mod module_bbb {
                        fn function_aaa() -> bool {
                            mod module_ccc {
                                fn function_bbb() -> string {
                                    "aaa"
                                }
                            }

                            true
                        }
                    }

                    fn function_ccc() -> int {
                        30
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    return; //: !
                }
                mod module_aaa {
                    mod module_bbb {
                        fn function_aaa() -> bool {
                            mod module_ccc {
                                fn function_bbb() -> string {
                                    expr:"aaa" //: string
                                }
                            }
                            expr:true //: bool
                        }
                    }

                    fn function_ccc() -> int {
                        expr:30 //: int
                    }
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn nested_outline_module() {
        check_pod_start_with_root_file(
            r#"
                //- /main.nail
                mod mod_aaa;

                fn main() -> int {
                    mod_aaa::fn_aaa();
                    mod_aaa::mod_bbb::fn_bbb()
                }

                //- /mod_aaa.nail
                mod mod_bbb;
                fn fn_aaa() -> int {
                    mod_bbb::fn_bbb()
                }

                //- /mod_aaa/mod_bbb.nail
                fn fn_bbb() -> int {
                    10
                }
            "#,
            expect![[r#"
                //- /main.nail
                mod mod_aaa;
                fn entry:main() -> int {
                    fn:mod_aaa::fn_aaa(); //: int
                    expr:fn:mod_aaa::mod_bbb::fn_bbb() //: int
                }

                //- /mod_aaa.nail
                mod mod_bbb;
                fn fn_aaa() -> int {
                    expr:fn:mod_bbb::fn_bbb() //: int
                }

                //- /mod_aaa/mod_bbb.nail
                fn fn_bbb() -> int {
                    expr:10 //: int
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn call_callable_expr() {
        check_in_root_file(
            r#"
                fn main() {
                    let a = 10;
                    a();
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    let a = 10; //: int
                    a(); //: <unknown>
                }

                ---
                error NotCallable: found_callee_ty: int, found_callee_symbol: a
                ---
                error Type is unknown: expr: a()
            "#]],
        );
    }

    #[test]
    fn mismatched_arg_len() {
        check_pod_start_with_root_file(
            r#"
                //- /main.nail
                fn main() {
                    callee(10);
                    callee(10, "a");
                    callee(10, "a", 30);
                }
                fn callee(a: int, b: string) -> int {
                    10
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    fn:callee(10); //: int
                    fn:callee(10, "a"); //: int
                    fn:callee(10, "a", 30); //: int
                }
                fn callee(a: int, b: string) -> int {
                    expr:10 //: int
                }

                ---
                error MismatchedCallArgCount: expected_arg_count: 2, found_arg_count: 1
                error MismatchedCallArgCount: expected_arg_count: 2, found_arg_count: 3
                ---
            "#]],
        );
    }

    #[test]
    fn symbol_to_module() {
        check_pod_start_with_root_file(
            r#"
                //- /main.nail
                fn main() {
                    aaa;
                    aaa::bbb;
                }

                mod aaa {
                    mod bbb {
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    mod:aaa; //: <unknown>
                    mod:aaa::bbb; //: <unknown>
                }
                mod aaa {
                    mod bbb {
                    }
                }

                ---
                error ModuleAsExpr: found_module: aaa
                error ModuleAsExpr: found_module: bbb
                ---
                error Type is unknown: expr: mod:aaa
                error Type is unknown: expr: mod:aaa::bbb
            "#]],
        );
    }

    /// unimplements hir
    #[test]
    fn symbol_to_use_item() {
        check_pod_start_with_root_file(
            r#"
                //- /main.nail
                mod aaa;
                use aaa:bbb;
                fn main() -> int {
                    bbb()
                }

                //- /aaa.nail
                mod aaa {
                    fn bbb() -> int {
                        10
                    }
                }
            "#,
            expect![[r#"
                //- /main.nail
                mod aaa;
                use aaa;
                fn entry:main() -> int {
                    expr:<missing>() //: <unknown>
                }

                //- /aaa.nail
                mod aaa {
                    fn bbb() -> int {
                        expr:10 //: int
                    }
                }

                ---
                error NotCallable: found_callee_ty: <unknown>, found_callee_symbol: <missing>
                error MismatchedTypeReturnValue: expected_ty: int, found_ty: <unknown>, found_expr: `<missing>()`
                ---
                error Type is unknown: expr: <missing>()
            "#]],
        );
    }
}
