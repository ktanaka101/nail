use std::sync::{Arc, Mutex};

use hir::{Expr, Function, HirFile, Symbol};
use salsa::DebugWithDb;

use crate::{
    BreakKind, HirTyMasterDatabase, InferenceError, Monotype, Signature, TyLowerResult,
    TypeCheckError,
};

/// SalsaのDB
#[derive(Default)]
#[salsa::db(hir::Jar, crate::Jar)]
pub struct TestingDatabase {
    storage: salsa::Storage<Self>,

    /// テスト用のログ
    logs: Option<Arc<Mutex<Vec<String>>>>,
}

impl TestingDatabase {
    #[cfg(test)]
    #[allow(dead_code)]
    pub fn enable_logging(self) -> Self {
        assert!(self.logs.is_none());
        Self {
            storage: self.storage,
            logs: Some(Default::default()),
        }
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub fn take_logs(&mut self) -> Vec<String> {
        if let Some(logs) = &self.logs {
            std::mem::take(&mut *logs.lock().unwrap())
        } else {
            panic!("logs not enabled");
        }
    }
}

impl salsa::Database for TestingDatabase {
    fn salsa_event(&self, event: salsa::Event) {
        if let Some(logs) = &self.logs {
            logs.lock()
                .unwrap()
                .push(format!("Event: {:?}", event.debug(self)));
        }
    }
}

fn indent(nesting: usize) -> String {
    "    ".repeat(nesting)
}

pub struct TestingDebug<'a> {
    db: &'a dyn HirTyMasterDatabase,
    pods: &'a hir::Pods,
    ty_lower_result: &'a TyLowerResult,
}
impl<'a> TestingDebug<'a> {
    pub fn new(
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

    pub fn debug(&self) -> String {
        let mut msg = "".to_string();

        msg.push_str(&self.debug_hir_file(self.pods.root_pod.root_hir_file, None));
        msg.push('\n');

        for (_nail_file, hir_file) in self.pods.root_pod.get_hir_files_order_registration_asc() {
            msg.push_str(&self.debug_hir_file(*hir_file, None));
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
                                "error MismatchedBinaryInteger: op: {op}, expected_ty: {}, found_ty: {}, found_expr: `{}`",
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
                                "error MismatchedBinaryCompare: op: {op}, expected_ty: {}, found_ty: {}, expected_expr: `{}`, found_expr: `{}`",
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
                    TypeCheckError::ImmutableReassignment { expr } => {
                        msg.push_str(&format!(
                            "error ImmutableReassignment: expr: {}",
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

    fn debug_hir_file(&self, hir_file: hir::HirFile, function: Option<hir::Function>) -> String {
        let mut msg = format!(
            "//- {}\n",
            hir_file.file(self.db).file_path(self.db).to_str().unwrap()
        );

        for item in hir_file.top_level_items(self.db) {
            msg.push_str(&self.debug_item(hir_file, function, *item, 0));
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
                let param_data = param.data(hir_file.db(self.db));
                self.format_parameter(hir_file, Some(function), param_data)
            })
            .collect::<Vec<_>>()
            .join(", ");
        let return_type = self.format_type(
            hir_file,
            Some(function),
            &function.return_type(self.db),
            nesting,
        );

        let hir::Expr::Block(block) = body_expr else {
            panic!("Should be Block")
        };

        let mut body = "{\n".to_string();
        for stmt in &block.stmts {
            body.push_str(&self.debug_stmt(hir_file, Some(function), stmt, nesting + 1));
        }
        if let Some(tail) = block.tail {
            let indent = indent(nesting + 1);
            body.push_str(&format!(
                "{indent}expr:{}",
                self.debug_expr(hir_file, Some(function), tail, nesting + 1)
            ));
            body.push_str(&format!(
                " {}\n",
                self.debug_type_line(Some(function), tail)
            ));
        }
        body.push_str(&format!("{}}}", indent(nesting)));

        let is_entry_point = hir_file.entry_point(self.db) == Some(function);
        format!(
            "{}fn {}{name}({params}) -> {return_type} {body}\n",
            indent(nesting),
            if is_entry_point { "entry:" } else { "" }
        )
    }

    fn debug_struct(
        &self,
        hir_file: HirFile,
        function: Option<hir::Function>,
        struct_: hir::Struct,
        nesting: usize,
    ) -> String {
        let name = struct_.name(self.db).text(self.db);
        let kind = match struct_.kind(self.db) {
            hir::StructKind::Tuple(fields) => {
                let fields = fields
                    .iter()
                    .map(|field| self.format_type(hir_file, function, field, nesting))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({fields});")
            }
            hir::StructKind::Named(fields) => {
                let fields = fields
                    .iter()
                    .map(|field| {
                        let name = field.name.text(self.db);
                        let ty = self.format_type(hir_file, function, &field.ty, nesting);
                        format!("{name}: {ty}")
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{{ {fields} }}", fields = fields)
            }
            hir::StructKind::Unit => ";".to_string(),
        };

        format!("{indent}struct {name}{kind}\n", indent = indent(nesting))
    }

    fn debug_module(
        &self,
        hir_file: hir::HirFile,
        function: Option<hir::Function>,
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
                    module_str.push_str(&self.debug_item(hir_file, function, *item, nesting + 1));
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

    fn debug_item(
        &self,
        hir_file: hir::HirFile,
        function: Option<hir::Function>,
        item: hir::Item,
        nesting: usize,
    ) -> String {
        match item {
            hir::Item::Function(function) => self.debug_function(hir_file, function, nesting),
            hir::Item::Struct(struct_) => self.debug_struct(hir_file, function, struct_, nesting),
            hir::Item::Module(module) => self.debug_module(hir_file, function, module, nesting),
            hir::Item::UseItem(use_item) => self.debug_use_item(use_item),
        }
    }

    fn debug_stmt(
        &self,
        hir_file: hir::HirFile,
        function: Option<hir::Function>,
        stmt: &hir::Stmt,
        nesting: usize,
    ) -> String {
        match stmt {
            hir::Stmt::Let {
                name,
                binding,
                value,
            } => {
                let indent = indent(nesting);
                let name = name.text(self.db);
                let expr_str = self.debug_expr(hir_file, function, *value, nesting);
                let binding = binding.lookup(hir_file.db(self.db));
                let mut_text = hir::testing::Pretty::format_mutable(binding.mutable);
                let mut stmt_str = format!("{indent}let {mut_text}{name} = {expr_str};");

                let type_line = self.debug_type_line(function, *value);
                stmt_str.push_str(&format!(" {type_line}\n"));

                stmt_str
            }
            hir::Stmt::Expr {
                expr,
                has_semicolon,
            } => {
                let indent = indent(nesting);
                let expr_str = self.debug_expr(hir_file, function, *expr, nesting);
                let type_line = self.debug_type_line(function, *expr);
                let maybe_semicolon = if *has_semicolon { ";" } else { "" };
                format!("{indent}{expr_str}{maybe_semicolon} {type_line}\n")
            }
            hir::Stmt::Item { item } => self.debug_item(hir_file, function, *item, nesting),
        }
    }

    fn debug_type_line(&self, function: Option<hir::Function>, expr_id: hir::ExprId) -> String {
        if let Some(function) = function {
            let ty = self
                .ty_lower_result
                .inference_body_by_function(function)
                .unwrap()
                .type_by_expr
                .get(&expr_id)
                .unwrap();

            format!("//: {}", self.debug_monotype(ty))
        } else {
            "//: no result".to_string()
        }
    }

    fn debug_monotype(&self, monotype: &Monotype) -> String {
        match monotype {
            Monotype::Integer => "int".to_string(),
            Monotype::Bool => "bool".to_string(),
            Monotype::Unit => "()".to_string(),
            Monotype::Char => "char".to_string(),
            Monotype::String => "string".to_string(),
            Monotype::Struct(struct_) => struct_.name(self.db).text(self.db).to_string(),
            Monotype::Variable(id) => format!("${}", id),
            Monotype::Function(signature) => self.debug_signature(signature),
            Monotype::Never => "!".to_string(),
            Monotype::Unknown => "<unknown>".to_string(),
            Monotype::UnknownCustom(_) => "<unknown>".to_string(),
        }
    }

    fn debug_expr(
        &self,
        hir_file: hir::HirFile,
        function: Option<hir::Function>,
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
                let op = op.to_string();
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
            hir::Symbol::Local { name, binding: _ } => name.text(self.db).to_string(),
            hir::Symbol::Param { name, .. } => {
                let name = name.text(self.db);
                format!("param:{name}")
            }
            hir::Symbol::MissingExpr { path } => {
                let resolving_status = self.pods.resolution_map.item_by_symbol(path).unwrap();
                self.debug_resolution_status(resolving_status)
            }
            hir::Symbol::MissingType { path } => {
                let resolving_status = self.pods.resolution_map.item_by_symbol(path).unwrap();
                self.debug_resolution_status(resolving_status)
            }
        }
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
                    hir::Item::Struct(_) => {
                        format!("struct:{path}")
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

    fn format_type(
        &self,
        hir_file: HirFile,
        function: Option<hir::Function>,
        ty: &hir::Type,
        nesting: usize,
    ) -> String {
        match ty {
            hir::Type::Integer => "int",
            hir::Type::String => "string",
            hir::Type::Char => "char",
            hir::Type::Boolean => "bool",
            hir::Type::Unit => "()",
            hir::Type::Unknown => "<unknown>",
            hir::Type::Custom(symbol) => {
                return self.format_symbol(hir_file, function, symbol, nesting);
            }
        }
        .to_string()
    }

    pub fn format_symbol(
        &self,
        hir_file: HirFile,
        function: Option<hir::Function>,
        symbol: &Symbol,
        nesting: usize,
    ) -> String {
        match &symbol {
            Symbol::Local { name, binding } => {
                let expr = binding.lookup(hir_file.db(self.db)).expr;
                match expr.lookup(hir_file.db(self.db)) {
                    Expr::Symbol { .. }
                    | Expr::Binary { .. }
                    | Expr::Missing
                    | Expr::Literal(_)
                    | Expr::Unary { .. }
                    | Expr::Call { .. }
                    | Expr::If { .. }
                    | Expr::Return { .. }
                    | Expr::Loop { .. }
                    | Expr::Continue
                    | Expr::Break { .. } => {
                        let expr_text = self.debug_expr(hir_file, function, expr, nesting);
                        format!("${}:{}", name.text(self.db), expr_text)
                    }
                    Expr::Block { .. } => name.text(self.db).to_string(),
                }
            }
            Symbol::Param { name, .. } => {
                let name = name.text(self.db);
                format!("param:{name}")
            }
            Symbol::MissingExpr { path } | Symbol::MissingType { path } => {
                let resolving_status = self.pods.resolution_map.item_by_symbol(path).unwrap();
                self.debug_resolution_status(resolving_status)
            }
        }
    }

    fn format_parameter(
        &self,
        hir_file: HirFile,
        function: Option<Function>,
        param_data: &hir::ParamData,
    ) -> String {
        let name = if let Some(name) = param_data.name {
            name.text(self.db)
        } else {
            "<missing>"
        };
        let mutable_text = hir::testing::Pretty::format_mutable(param_data.mutable);
        let ty = self.format_type(hir_file, function, &param_data.ty, 0);
        format!("{name}: {mutable_text}{ty}")
    }
}
