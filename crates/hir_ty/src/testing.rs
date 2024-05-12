use std::sync::{Arc, Mutex};

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

pub struct Pretty<'a> {
    db: &'a dyn HirTyMasterDatabase,
    pods: &'a hir::Pods,
    ty_lower_result: &'a TyLowerResult,
}
impl<'a> Pretty<'a> {
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

    pub fn format(&self) -> String {
        let mut msg = "".to_string();

        msg.push_str(&self.format_hir_file(self.pods.root_pod.root_hir_file, None));
        msg.push('\n');

        for (_nail_file, hir_file) in self.pods.root_pod.get_hir_files_order_registration_asc() {
            msg.push_str(&self.format_hir_file(*hir_file, None));
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
                                self.format_monotype(expected_condition_bool_ty),
                                self.format_monotype(found_condition_ty),
                                self.format_simplify_expr(hir_file, *found_condition_expr),
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
                                self.format_monotype(then_branch_ty),
                                self.format_monotype(else_branch_ty),
                                self.format_simplify_expr(hir_file, *then_branch),
                                self.format_simplify_expr(hir_file, *else_branch),
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
                                self.format_monotype(then_branch_ty),
                                self.format_monotype(else_branch_unit_ty),
                                self.format_simplify_expr(hir_file, *then_branch),
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
                                self.format_monotype(expected_ty),
                                self.format_monotype(found_ty),
                                self.format_simplify_expr(hir_file, *found_expr),
                                self.format_signature(expected_signature),
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
                                self.format_monotype(expected_int_ty),
                                self.format_monotype(found_ty),
                                self.format_simplify_expr(hir_file, *found_expr),
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
                                self.format_monotype(compare_from_ty),
                                self.format_monotype(compare_to_ty),
                                self.format_simplify_expr(hir_file, *compare_from_expr),
                                self.format_simplify_expr(hir_file, *compare_to_expr),
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
                                self.format_unary_op(*op),
                                self.format_monotype(expected_ty),
                                self.format_monotype(found_ty),
                                self.format_simplify_expr(hir_file, *found_expr),
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
                                self.format_monotype(&expected_signature.return_type(self.db)),
                                self.format_monotype(found_ty),
                                self.format_simplify_expr(hir_file, *found_return_expr),
                            ));
                        } else {
                            msg.push_str(&format!(
                                "error MismatchedReturnType: expected_ty: {}, found_ty: {}",
                                self.format_monotype(&expected_signature.return_type(self.db)),
                                self.format_monotype(found_ty),
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
                                self.format_monotype(&expected_signature.return_type(self.db)),
                                self.format_monotype(found_ty),
                                self.format_simplify_expr(hir_file, *found_last_expr),
                            ));
                        } else {
                            msg.push_str(&format!(
                                "error MismatchedTypeReturnValue: expected_ty: {}, found_ty: {}",
                                self.format_monotype(&expected_signature.return_type(self.db)),
                                self.format_monotype(found_ty),
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
                            self.format_monotype(found_callee_ty),
                            self.format_symbol(found_callee_symbol),
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
                                    self.format_monotype(expected_ty),
                                    self.format_monotype(found_ty),
                                    self.format_simplify_expr(hir_file, *expected_expr),
                                    self.format_simplify_expr(hir_file, *found_expr),
                                ));
                        } else {
                            msg.push_str(
                                &format!(
                                "error MismatchedType: expected_ty: {}, found_ty: {}, found_expr: `{}`",
                                self.format_monotype(expected_ty),
                                self.format_monotype(found_ty),
                                self.format_simplify_expr(hir_file, *found_expr),
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
                            self.format_simplify_expr(hir_file, *found_expr),
                        ));
                    }
                    InferenceError::NotRecord {
                        found_struct_ty,
                        found_struct_symbol,
                        found_expr,
                    } => {
                        msg.push_str(&format!(
                            "error NotRecord: found_struct_ty: {}, found_struct_symbol: {}, found_expr: `{}`",
                            self.format_monotype(found_struct_ty),
                            self.format_symbol(found_struct_symbol),
                            self.format_simplify_expr(hir_file, *found_expr),
                        ));
                    }
                    InferenceError::NotAllowedType {
                        found_symbol,
                        found_function,
                    } => {
                        msg.push_str(&format!(
                            "error NotAllowedType: found_symbol: {}, found_function: {}",
                            self.format_symbol(found_symbol),
                            found_function.name(self.db).text(self.db),
                        ));
                    }
                    InferenceError::MismatchedTypeInitStructTuple {
                        expected_ty,
                        found_ty,
                        init_struct,
                        found_arg_expr,
                        arg_pos,
                        found_expr,
                    } => {
                        msg.push_str(&format!(
                            "error MismatchedTypeInitStructTuple: expected_ty: {}, found_ty: {}, init_struct: {}, found_arg_expr: `{}`, arg_pos: {}, found_expr: `{}`",
                            self.format_monotype(expected_ty),
                            self.format_monotype(found_ty),
                            init_struct.name(self.db).text(self.db),
                            self.format_simplify_expr(hir_file, *found_arg_expr),
                            arg_pos,
                            self.format_simplify_expr(hir_file, *found_expr),
                        ));
                    }
                    InferenceError::MissingStructRecordField {
                        missing_fields,
                        found_struct,
                        found_expr,
                    } => {
                        msg.push_str(&format!(
                            "error MissingStructRecordField: missing_fields: {:?}, found_struct: {}, found_expr: `{}`",
                            missing_fields
                                .iter()
                                .map(|field| field.text(self.db))
                                .collect::<Vec<_>>(),
                            found_struct.name(self.db).text(self.db),
                            self.format_simplify_expr(hir_file, *found_expr),
                        ));
                    }
                    InferenceError::NoSuchStructRecordField {
                        no_such_fields,
                        found_struct,
                        found_expr,
                    } => {
                        msg.push_str(&format!(
                            "error NoSuchStructRecordField: no_such_fields: {:?}, found_struct: {}, found_expr: `{}`",
                            no_such_fields
                                .iter()
                                .map(|field| field.name.text(self.db))
                                .collect::<Vec<_>>(),
                            found_struct.name(self.db).text(self.db),
                            self.format_simplify_expr(hir_file, *found_expr),
                        ));
                    }
                    InferenceError::MismatchedTypeInitStructRecord {
                        expected_ty,
                        found_ty,
                        found_struct: init_struct,
                        found_name,
                        found_expr,
                    } => {
                        msg.push_str(&format!(
                            "error MismatchedTypeInitStructRecord: expected_ty: {}, found_ty: {}, init_struct: {}, found_name: {}, found_expr: `{}`",
                            self.format_monotype(expected_ty),
                            self.format_monotype(found_ty),
                            init_struct.name(self.db).text(self.db),
                            found_name.text(self.db),
                            self.format_simplify_expr(hir_file, *found_expr),
                        ));
                    }
                    InferenceError::NeededInitTupleOrRecord {
                        found_ty,
                        found_expr,
                        found_struct,
                    } => {
                        msg.push_str(&format!(
                            "error NeededInitTupleOrRecord: found_ty: {}, found_expr: `{}`, found_struct: {}",
                            self.format_monotype(found_ty),
                            self.format_simplify_expr(hir_file, *found_expr),
                            found_struct.name(self.db).text(self.db),
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
                            self.format_simplify_expr(hir_file, *expr),
                        ));
                    }
                    TypeCheckError::ImmutableReassignment { expr } => {
                        msg.push_str(&format!(
                            "error ImmutableReassignment: expr: {}",
                            self.format_simplify_expr(hir_file, *expr),
                        ));
                    }
                }
                msg.push('\n');
            }
        }

        msg
    }

    fn format_signature(&self, signature: &Signature) -> String {
        let params = signature
            .params(self.db)
            .iter()
            .map(|param| self.format_monotype(param))
            .collect::<Vec<_>>()
            .join(", ");
        let return_type = self.format_monotype(&signature.return_type(self.db));

        format!("({params}) -> {return_type}")
    }

    fn format_hir_file(&self, hir_file: hir::HirFile, function: Option<hir::Function>) -> String {
        let mut msg = format!(
            "//- {}\n",
            hir_file.file(self.db).file_path(self.db).to_str().unwrap()
        );

        for item in hir_file.top_level_items(self.db) {
            msg.push_str(&self.format_item(hir_file, function, *item, 0));
        }

        msg
    }

    fn format_function(
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
                self.format_parameter(param_data)
            })
            .collect::<Vec<_>>()
            .join(", ");
        let return_type = self.format_type(&function.return_type(self.db));

        let hir::Expr::Block(block) = body_expr else {
            panic!("Should be Block")
        };

        let mut body = "{\n".to_string();
        for stmt in &block.stmts {
            body.push_str(&self.format_stmt(hir_file, Some(function), stmt, nesting + 1));
        }
        if let Some(tail) = block.tail {
            let indent = hir::testing::Pretty::format_indent(nesting + 1);
            body.push_str(&format!(
                "{indent}expr:{}",
                self.format_expr(hir_file, Some(function), tail, nesting + 1)
            ));
            body.push_str(&format!(
                " {}\n",
                self.format_type_line(Some(function), tail)
            ));
        }
        body.push_str(&format!(
            "{}}}",
            hir::testing::Pretty::format_indent(nesting)
        ));

        let is_entry_point = hir_file.entry_point(self.db) == Some(function);
        format!(
            "{}fn {}{name}({params}) -> {return_type} {body}\n",
            hir::testing::Pretty::format_indent(nesting),
            if is_entry_point { "entry:" } else { "" }
        )
    }

    fn format_struct(&self, struct_: hir::Struct, nesting: usize) -> String {
        let name = struct_.name(self.db).text(self.db);
        let kind = match struct_.kind(self.db) {
            hir::StructKind::Tuple(fields) => {
                let fields = fields
                    .iter()
                    .map(|field| self.format_type(field))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({fields});")
            }
            hir::StructKind::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|field| {
                        let name = field.name.text(self.db);
                        let ty = self.format_type(&field.ty);
                        format!("{name}: {ty}")
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{{ {fields} }}", fields = fields)
            }
            hir::StructKind::Unit => ";".to_string(),
        };

        format!(
            "{indent}struct {name}{kind}\n",
            indent = hir::testing::Pretty::format_indent(nesting)
        )
    }

    fn format_module(
        &self,
        hir_file: hir::HirFile,
        function: Option<hir::Function>,
        module: hir::Module,
        nesting: usize,
    ) -> String {
        let _scope_origin = hir::ModuleScopeOrigin::Module { origin: module };

        let curr_indent = hir::testing::Pretty::format_indent(nesting);

        let module_name = module.name(self.db).text(self.db);

        match module.kind(self.db) {
            hir::ModuleKind::Inline { items } => {
                let mut module_str = "".to_string();
                module_str.push_str(&format!("{curr_indent}mod {module_name} {{\n"));
                for (i, item) in items.iter().enumerate() {
                    module_str.push_str(&self.format_item(hir_file, function, *item, nesting + 1));
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

    fn format_use_item(&self, use_item: hir::UseItem) -> String {
        let path_name = self.format_path(use_item.path(self.db));
        let item_name = use_item.name(self.db).text(self.db);

        if path_name.is_empty() {
            format!("use {item_name};\n")
        } else {
            format!("use {path_name}::{item_name};\n")
        }
    }

    fn format_item(
        &self,
        hir_file: hir::HirFile,
        function: Option<hir::Function>,
        item: hir::Item,
        nesting: usize,
    ) -> String {
        match item {
            hir::Item::Function(function) => self.format_function(hir_file, function, nesting),
            hir::Item::Struct(struct_) => self.format_struct(struct_, nesting),
            hir::Item::Module(module) => self.format_module(hir_file, function, module, nesting),
            hir::Item::UseItem(use_item) => self.format_use_item(use_item),
        }
    }

    fn format_stmt(
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
                let indent = hir::testing::Pretty::format_indent(nesting);
                let name = name.text(self.db);
                let expr_str = self.format_expr(hir_file, function, *value, nesting);
                let binding = binding.lookup(hir_file.db(self.db));
                let mut_text = hir::testing::Pretty::format_mutable(binding.mutable);
                let mut stmt_str = format!("{indent}let {mut_text}{name} = {expr_str};");

                let type_line = self.format_type_line(function, *value);
                stmt_str.push_str(&format!(" {type_line}\n"));

                stmt_str
            }
            hir::Stmt::Expr {
                expr,
                has_semicolon,
            } => {
                let indent = hir::testing::Pretty::format_indent(nesting);
                let expr_str = self.format_expr(hir_file, function, *expr, nesting);
                let type_line = self.format_type_line(function, *expr);
                let maybe_semicolon = if *has_semicolon { ";" } else { "" };
                format!("{indent}{expr_str}{maybe_semicolon} {type_line}\n")
            }
            hir::Stmt::Item { item } => self.format_item(hir_file, function, *item, nesting),
        }
    }

    fn format_type_line(&self, function: Option<hir::Function>, expr_id: hir::ExprId) -> String {
        if let Some(function) = function {
            let ty = self
                .ty_lower_result
                .inference_body_by_function(function)
                .unwrap()
                .type_by_expr
                .get(&expr_id)
                .unwrap();

            format!("//: {}", self.format_monotype(ty))
        } else {
            "//: no result".to_string()
        }
    }

    fn format_monotype(&self, monotype: &Monotype) -> String {
        match monotype {
            Monotype::Integer => "int".to_string(),
            Monotype::Bool => "bool".to_string(),
            Monotype::Unit => "()".to_string(),
            Monotype::Char => "char".to_string(),
            Monotype::String => "string".to_string(),
            Monotype::Struct(struct_) => struct_.name(self.db).text(self.db).to_string(),
            Monotype::Variable(id) => format!("${}", id),
            Monotype::Function(signature) => self.format_signature(signature),
            Monotype::Never => "!".to_string(),
            Monotype::Unknown => "<unknown>".to_string(),
        }
    }

    fn format_expr(
        &self,
        hir_file: hir::HirFile,
        function: Option<hir::Function>,
        expr_id: hir::ExprId,
        nesting: usize,
    ) -> String {
        match expr_id.lookup(hir_file.db(self.db)) {
            hir::Expr::Symbol(symbol) => self.format_symbol(symbol),
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Bool(b) => b.to_string(),
                hir::Literal::Char(c) => format!("'{c}'"),
                hir::Literal::String(s) => format!("\"{s}\""),
                hir::Literal::Integer(i) => i.to_string(),
            },
            hir::Expr::Binary { op, lhs, rhs } => {
                let lhs_str = self.format_expr(hir_file, function, *lhs, nesting);
                let rhs_str = self.format_expr(hir_file, function, *rhs, nesting);
                format!("{lhs_str} {op} {rhs_str}")
            }
            hir::Expr::Unary { op, expr } => {
                let op = match op {
                    hir::UnaryOp::Neg => "-",
                    hir::UnaryOp::Not => "!",
                };
                let expr_str = self.format_expr(hir_file, function, *expr, nesting);
                format!("{op}{expr_str}")
            }
            hir::Expr::Call { callee, args } => {
                let callee = self.format_symbol(callee);
                let args = args
                    .iter()
                    .map(|arg| self.format_expr(hir_file, function, *arg, nesting))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{callee}({args})")
            }
            hir::Expr::Block(block) => {
                let mut msg = "{\n".to_string();
                for stmt in &block.stmts {
                    msg.push_str(&self.format_stmt(hir_file, function, stmt, nesting + 1));
                }
                if let Some(tail) = block.tail {
                    let indent = hir::testing::Pretty::format_indent(nesting + 1);
                    msg.push_str(&format!(
                        "{indent}expr:{}",
                        self.format_expr(hir_file, function, tail, nesting + 1)
                    ));
                    msg.push_str(&format!(" {}\n", self.format_type_line(function, tail)));
                }
                msg.push_str(&format!(
                    "{}}}",
                    hir::testing::Pretty::format_indent(nesting)
                ));

                msg
            }
            hir::Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut msg = "if ".to_string();
                msg.push_str(&self.format_expr(hir_file, function, *condition, nesting));
                msg.push(' ');
                msg.push_str(&self.format_expr(hir_file, function, *then_branch, nesting));

                if let Some(else_branch) = else_branch {
                    msg.push_str(" else ");
                    msg.push_str(&self.format_expr(hir_file, function, *else_branch, nesting));
                }

                msg
            }
            hir::Expr::Return { value } => {
                let mut msg = "return".to_string();
                if let Some(value) = value {
                    msg.push_str(&format!(
                        " {}",
                        &self.format_expr(hir_file, function, *value, nesting,)
                    ));
                }

                msg
            }
            hir::Expr::Loop { block } => {
                let mut msg = "loop ".to_string();
                msg.push_str(&self.format_expr(hir_file, function, *block, nesting));

                msg
            }
            hir::Expr::Continue => "continue".to_string(),
            hir::Expr::Break { value } => {
                let mut msg = "break".to_string();
                if let Some(value) = value {
                    msg.push_str(&format!(
                        " {}",
                        &self.format_expr(hir_file, function, *value, nesting,)
                    ));
                }

                msg
            }
            hir::Expr::Record { symbol, fields } => {
                let symbol = self.format_symbol(symbol);
                let fields = fields
                    .iter()
                    .map(|field| {
                        let name = field.name.text(self.db);
                        let value = self.format_expr(hir_file, function, field.value, nesting);
                        format!("{name}: {value}")
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{symbol} {{ {fields} }}")
            }
            hir::Expr::Missing => "<missing>".to_string(),
        }
    }

    fn format_simplify_expr(&self, hir_file: hir::HirFile, expr_id: hir::ExprId) -> String {
        match expr_id.lookup(hir_file.db(self.db)) {
            hir::Expr::Symbol(symbol) => self.format_symbol(symbol),
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Bool(b) => b.to_string(),
                hir::Literal::Char(c) => format!("'{c}'"),
                hir::Literal::String(s) => format!("\"{s}\""),
                hir::Literal::Integer(i) => i.to_string(),
            },
            hir::Expr::Binary { op, lhs, rhs } => {
                let op = op.to_string();
                let lhs_str = self.format_simplify_expr(hir_file, *lhs);
                let rhs_str = self.format_simplify_expr(hir_file, *rhs);
                format!("{lhs_str} {op} {rhs_str}")
            }
            hir::Expr::Unary { op, expr } => {
                let op = match op {
                    hir::UnaryOp::Neg => "-",
                    hir::UnaryOp::Not => "!",
                };
                let expr_str = self.format_simplify_expr(hir_file, *expr);
                format!("{op}{expr_str}")
            }
            hir::Expr::Call { callee, args } => {
                let callee = self.format_symbol(callee);
                let args = args
                    .iter()
                    .map(|arg| self.format_simplify_expr(hir_file, *arg))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{callee}({args})")
            }
            hir::Expr::Block(block) => {
                let mut msg = "{ tail:".to_string();
                if let Some(tail) = block.tail {
                    msg.push_str(&self.format_simplify_expr(hir_file, tail));
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
                let cond_str = self.format_simplify_expr(hir_file, *condition);
                let mut msg = format!(
                    "if {cond_str} {}",
                    self.format_simplify_expr(hir_file, *then_branch)
                );

                if let Some(else_branch) = else_branch {
                    msg.push_str(&format!(
                        " else {}",
                        self.format_simplify_expr(hir_file, *else_branch)
                    ));
                }

                msg
            }
            hir::Expr::Return { value } => {
                let mut msg = "return".to_string();
                if let Some(value) = value {
                    msg.push_str(&format!(
                        " {}",
                        &self.format_simplify_expr(hir_file, *value,)
                    ));
                }

                msg
            }
            hir::Expr::Loop { block } => {
                let mut msg = "loop ".to_string();
                msg.push_str(&self.format_simplify_expr(hir_file, *block));

                msg
            }
            hir::Expr::Continue => "continue".to_string(),
            hir::Expr::Break { value } => {
                let mut msg = "break".to_string();
                if let Some(value) = value {
                    msg.push_str(&format!(
                        " {}",
                        &self.format_simplify_expr(hir_file, *value,)
                    ));
                }

                msg
            }
            hir::Expr::Record { symbol, fields } => {
                let symbol = self.format_symbol(symbol);
                let fields = fields
                    .iter()
                    .map(|field| {
                        let name = field.name.text(self.db);
                        let value = self.format_simplify_expr(hir_file, field.value);
                        format!("{name}: {value}")
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{symbol} {{ {fields} }}")
            }
            hir::Expr::Missing => "<missing>".to_string(),
        }
    }

    fn format_symbol(&self, symbol: &hir::Symbol) -> String {
        match &symbol {
            hir::Symbol::Local { name, binding: _ } => name.text(self.db).to_string(),
            hir::Symbol::Param { name, .. } => {
                let name = name.text(self.db);
                format!("param:{name}")
            }
            hir::Symbol::MissingExpr { path } => {
                let resolving_status = self.pods.resolution_map.item_by_symbol(path).unwrap();
                self.format_resolution_status(resolving_status)
            }
            hir::Symbol::MissingType { path } => {
                let resolving_status = self.pods.resolution_map.item_by_symbol(path).unwrap();
                self.format_resolution_status(resolving_status)
            }
        }
    }

    fn format_unary_op(&self, op: hir::UnaryOp) -> String {
        match op {
            hir::UnaryOp::Neg => "-",
            hir::UnaryOp::Not => "!",
        }
        .to_string()
    }

    fn format_resolution_status(&self, resolution_status: hir::ResolutionStatus) -> String {
        match resolution_status {
            hir::ResolutionStatus::Unresolved => "<unknown>".to_string(),
            hir::ResolutionStatus::Error => "<missing>".to_string(),
            hir::ResolutionStatus::Resolved { path, item } => {
                let path = self.format_path(&path);
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
                            self.format_resolution_status(item),
                            use_item.name(self.db).text(self.db)
                        )
                    }
                }
            }
        }
    }

    fn format_path(&self, path: &hir::Path) -> String {
        path.segments(self.db)
            .iter()
            .map(|segment| segment.text(self.db).to_string())
            .collect::<Vec<_>>()
            .join("::")
    }

    fn format_type(&self, ty: &hir::Type) -> String {
        match ty {
            hir::Type::Integer => "int",
            hir::Type::String => "string",
            hir::Type::Char => "char",
            hir::Type::Boolean => "bool",
            hir::Type::Unit => "()",
            hir::Type::Unknown => "<unknown>",
            hir::Type::Custom(symbol) => {
                return self.format_symbol(symbol);
            }
        }
        .to_string()
    }

    fn format_parameter(&self, param_data: &hir::ParamData) -> String {
        let name = if let Some(name) = param_data.name {
            name.text(self.db)
        } else {
            "<missing>"
        };
        let mutable_text = hir::testing::Pretty::format_mutable(param_data.mutable);
        let ty = self.format_type(&param_data.ty);
        format!("{name}: {mutable_text}{ty}")
    }
}
