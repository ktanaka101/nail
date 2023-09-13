//! HIRに型付けを行います。
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
//! 現時点の型推論は簡易なもので、Hindley-Milner型推論ベースに変更する予定です。

#![feature(trait_upcasting)]
// #[salsa::tracked]で生成される関数にドキュメントコメントが作成されないため警告が出てしまうため許可します。
// #![warn(missing_docs)]

mod checker;
mod inference;

pub use checker::{TypeCheckError, TypeCheckResult};
use inference::Signature;
pub use inference::{InferenceBodyResult, InferenceResult};

/// HIRを元にTypedHIRを構築します。
pub fn lower_pods(db: &dyn hir::HirMasterDatabase, pods: &hir::Pods) -> TyLowerResult {
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
    pub inference_result: InferenceResult,
    /// 型チェックの結果
    pub type_check_result: TypeCheckResult,
}
impl TyLowerResult {
    /// 指定した関数の型を取得します。
    pub fn signature_by_function(&self, function_id: hir::Function) -> &Signature {
        &self.inference_result.signature_by_function[&function_id]
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{inference::infer_pods, InferenceResult};

    fn check_pod_start_with_root_file(fixture: &str, expect: Expect) {
        let db = hir::TestingDatabase::default();
        let mut source_db = hir::FixtureDatabase::new(&db, fixture);

        let pods = hir::parse_pods(&db, "/main.nail", &mut source_db);
        let inference_result = infer_pods(&db, &pods);

        expect.assert_eq(&TestingDebug::new(&db, &pods, &inference_result).debug());
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
        db: &'a dyn hir::HirMasterDatabase,
        pods: &'a hir::Pods,
        inference_result: &'a InferenceResult,
    }
    impl<'a> TestingDebug<'a> {
        fn new(
            db: &'a dyn hir::HirMasterDatabase,
            pods: &'a hir::Pods,
            inference_result: &'a InferenceResult,
        ) -> Self {
            TestingDebug {
                db,
                pods,
                inference_result,
            }
        }

        fn debug(&self) -> String {
            let mut msg = "".to_string();

            msg.push_str(&self.debug_hir_file(self.pods.root_pod.root_hir_file));

            for (_nail_file, hir_file) in self.pods.root_pod.get_hir_files_order_registration_asc()
            {
                msg.push_str(&self.debug_hir_file(*hir_file));
                msg.push('\n');
            }

            msg.push_str("---\n");
            for (_hir_file, function) in self.pods.root_pod.all_functions(self.db) {
                let inference_body_result = self
                    .inference_result
                    .inference_body_result_by_function
                    .get(&function)
                    .unwrap();

                for error in &inference_body_result.errors {
                    match error {
                        crate::inference::InferenceError::TypeMismatch { expected, actual } => {
                            msg.push_str(&format!(
                                "error: expected {expected}, actual: {actual}\n"
                            ));
                        }
                    }
                }
            }

            msg
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
            let body_expr = hir_file
                .db(self.db)
                .function_body_by_ast_block(function.ast(self.db).body().unwrap())
                .unwrap();

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

            let scope_origin = hir::ModuleScopeOrigin::Function { origin: function };

            let hir::Expr::Block(block) = body_expr else { panic!("Should be Block") };

            let mut body = "{\n".to_string();
            for stmt in &block.stmts {
                body.push_str(&self.debug_stmt(
                    hir_file,
                    function,
                    scope_origin,
                    stmt,
                    nesting + 1,
                ));
            }
            if let Some(tail) = block.tail {
                let indent = indent(nesting + 1);
                body.push_str(&format!(
                    "{indent}expr:{}\n",
                    self.debug_expr(hir_file, function, scope_origin, tail, nesting + 1)
                ));
                body.push_str(&format!(
                    "{indent}{}\n",
                    self.debug_type_line(hir_file, function, scope_origin, tail)
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

            format!("{path_name}::{item_name}")
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
            scope_origin: hir::ModuleScopeOrigin,
            stmt: &hir::Stmt,
            nesting: usize,
        ) -> String {
            match stmt {
                hir::Stmt::VariableDef { name, value } => {
                    let indent = indent(nesting);
                    let name = name.text(self.db);
                    let expr_str =
                        self.debug_expr(hir_file, function, scope_origin, *value, nesting);
                    let mut stmt_str = format!("{indent}let {name} = {expr_str}\n");

                    let type_line = self.debug_type_line(hir_file, function, scope_origin, *value);
                    stmt_str.push_str(&format!("{indent}{type_line}\n"));

                    stmt_str
                }
                hir::Stmt::ExprStmt {
                    expr,
                    has_semicolon,
                } => {
                    let indent = indent(nesting);
                    let expr_str =
                        self.debug_expr(hir_file, function, scope_origin, *expr, nesting);
                    let type_line = self.debug_type_line(hir_file, function, scope_origin, *expr);
                    let maybe_semicolon = if *has_semicolon { ";" } else { "" };
                    format!("{indent}{expr_str}{maybe_semicolon}\n{indent}{type_line}\n")
                }
                hir::Stmt::Item { item } => self.debug_item(hir_file, *item, nesting),
            }
        }

        fn debug_type_line(
            &self,
            hir_file: hir::HirFile,
            function: hir::Function,
            scope_origin: hir::ModuleScopeOrigin,
            expr_id: hir::ExprId,
        ) -> String {
            let ty = self
                .inference_result
                .inference_body_result_by_function
                .get(&function)
                .unwrap()
                .type_by_expr
                .get(&expr_id)
                .unwrap();
            let expr_str = match expr_id.lookup(hir_file.db(self.db)) {
                hir::Expr::Symbol(_)
                | hir::Expr::Binary { .. }
                | hir::Expr::Literal(_)
                | hir::Expr::Unary { .. }
                | hir::Expr::Call { .. }
                | hir::Expr::If { .. }
                | hir::Expr::Return { .. }
                | hir::Expr::Missing => {
                    self.debug_expr(hir_file, function, scope_origin, expr_id, 0)
                }
                hir::Expr::Block(_) => "{ .. }".to_string(),
            };

            format!("// {expr_str}: {ty}")
        }

        fn debug_expr(
            &self,
            hir_file: hir::HirFile,
            function: hir::Function,
            scope_origin: hir::ModuleScopeOrigin,
            expr_id: hir::ExprId,
            nesting: usize,
        ) -> String {
            match expr_id.lookup(hir_file.db(self.db)) {
                hir::Expr::Symbol(symbol) => {
                    self.debug_symbol(hir_file, function, scope_origin, symbol, nesting)
                }
                hir::Expr::Literal(literal) => match literal {
                    hir::Literal::Bool(b) => b.to_string(),
                    hir::Literal::Char(c) => format!("'{c}'"),
                    hir::Literal::String(s) => format!("\"{s}\""),
                    hir::Literal::Integer(i) => i.to_string(),
                },
                hir::Expr::Binary { op, lhs, rhs } => {
                    let op = match op {
                        ast::BinaryOp::Add(_) => "+",
                        ast::BinaryOp::Sub(_) => "-",
                        ast::BinaryOp::Mul(_) => "*",
                        ast::BinaryOp::Div(_) => "/",
                        ast::BinaryOp::Equal(_) => "==",
                        ast::BinaryOp::GreaterThan(_) => ">",
                        ast::BinaryOp::LessThan(_) => "<",
                    };
                    let lhs_str = self.debug_expr(hir_file, function, scope_origin, *lhs, nesting);
                    let rhs_str = self.debug_expr(hir_file, function, scope_origin, *rhs, nesting);
                    format!("{lhs_str} {op} {rhs_str}")
                }
                hir::Expr::Unary { op, expr } => {
                    let op = match op {
                        ast::UnaryOp::Neg(_) => "-",
                        ast::UnaryOp::Not(_) => "!",
                    };
                    let expr_str =
                        self.debug_expr(hir_file, function, scope_origin, *expr, nesting);
                    format!("{op}{expr_str}")
                }
                hir::Expr::Call { callee, args } => {
                    let callee =
                        self.debug_symbol(hir_file, function, scope_origin, callee, nesting);
                    let args = args
                        .iter()
                        .map(|arg| self.debug_expr(hir_file, function, scope_origin, *arg, nesting))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{callee}({args})")
                }
                hir::Expr::Block(block) => {
                    let scope_origin = hir::ModuleScopeOrigin::Block { origin: expr_id };

                    let mut msg = "{\n".to_string();
                    for stmt in &block.stmts {
                        msg.push_str(&self.debug_stmt(
                            hir_file,
                            function,
                            scope_origin,
                            stmt,
                            nesting + 1,
                        ));
                    }
                    if let Some(tail) = block.tail {
                        let indent = indent(nesting + 1);
                        msg.push_str(&format!(
                            "{indent}expr:{}\n",
                            self.debug_expr(hir_file, function, scope_origin, tail, nesting + 1)
                        ));
                        msg.push_str(&format!(
                            "{indent}{}\n",
                            self.debug_type_line(hir_file, function, scope_origin, tail)
                        ));
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
                    msg.push_str(&self.debug_expr(
                        hir_file,
                        function,
                        scope_origin,
                        *condition,
                        nesting,
                    ));
                    msg.push(' ');
                    msg.push_str(&self.debug_expr(
                        hir_file,
                        function,
                        scope_origin,
                        *then_branch,
                        nesting,
                    ));

                    if let Some(else_branch) = else_branch {
                        msg.push_str(" else ");
                        msg.push_str(&self.debug_expr(
                            hir_file,
                            function,
                            scope_origin,
                            *else_branch,
                            nesting,
                        ));
                    }

                    msg
                }
                hir::Expr::Return { value } => {
                    let mut msg = "return".to_string();
                    if let Some(value) = value {
                        msg.push_str(&format!(
                            " {}",
                            &self.debug_expr(hir_file, function, scope_origin, *value, nesting,)
                        ));
                    }

                    msg
                }
                hir::Expr::Missing => "<missing>".to_string(),
            }
        }

        fn debug_symbol(
            &self,
            _hir_file: hir::HirFile,
            _function: hir::Function,
            _scope_origin: hir::ModuleScopeOrigin,
            symbol: &hir::Symbol,
            _nesting: usize,
        ) -> String {
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
                        hir::Item::UseItem(_) => {
                            unreachable!()
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
                fn(int) -> int
                fn() -> int
                ---
                `x`: int
                `0`: int
                `x == 0`: bool
                `0`: int
                `{ .., 0 }`: int
                `x`: int
                `1`: int
                `x == 1`: bool
                `1`: int
                `{ .., 1 }`: int
                `x`: int
                `1`: int
                `x - 1`: int
                `x`: int
                `2`: int
                `x - 2`: int
                `fibonacci(x - 1)`: int
                `fibonacci(x - 2)`: int
                `fibonacci(x - 1) + fibonacci(x - 2)`: int
                `{ .., fibonacci(x - 1) + fibonacci(x - 2) }`: int
                `if x == 1 { .., 1 } else { .., fibonacci(x - 1) + fibonacci(x - 2) }`: int
                `{ .., if x == 1 { .., 1 } else { .., fibonacci(x - 1) + fibonacci(x - 2) } }`: int
                `if x == 0 { .., 0 } else { .., if x == 1 { .., 1 } else { .., fibonacci(x - 1) + fibonacci(x - 2) } }`: int
                `15`: int
                `fibonacci(15)`: int
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
                    10;
                    // 10: integer
                }
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
                    "aaa";
                    // "aaa": string
                }
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
                    'a';
                    // 'a': char
                }
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
                    true;
                    // true: bool
                }
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
                    false;
                    // false: bool
                }
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
                    let a = true
                    // true: bool
                }
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
                    let a = true
                    // true: bool
                    let b = 10
                    // 10: integer
                    let c = "aa"
                    // "aa": string
                    let d = 'a'
                    // 'a': char
                }
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
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    10 + 20
                    // 10 + 20: integer
                    "aaa" + "bbb"
                    // "aaa" + "bbb": integer
                    10 + "aaa"
                    // 10 + "aaa": integer
                    'a' + 'a'
                    // 'a' + 'a': integer
                    10 + 'a'
                    // 10 + 'a': integer
                    10 < 'a'
                    // 10 < 'a': bool
                    10 > 'a'
                    // 10 > 'a': bool
                    true + true
                    // true + true: integer
                    true - true
                    // true - true: integer
                    true * true
                    // true * true: integer
                    true / true
                    // true / true: integer
                    true == true
                    // true == true: bool
                    true < false
                    // true < false: bool
                    true > false
                    // true > false: bool
                    10 + true
                    // 10 + true: integer
                    10 + 10 + "aaa"
                    // 10 + 10 + "aaa": integer
                    10 - 20
                    // 10 - 20: integer
                    10 * 20
                    // 10 * 20: integer
                    10 / 20
                    // 10 / 20: integer
                    10 == 20
                    // 10 == 20: bool
                    10 < 20
                    // 10 < 20: bool
                    10 > 20;
                    // 10 > 20: bool
                }
                ---
                error: expected integer, actual: string
                error: expected integer, actual: string
                error: expected integer, actual: string
                error: expected integer, actual: char
                error: expected integer, actual: char
                error: expected integer, actual: char
                error: expected integer, actual: char
                error: expected integer, actual: char
                error: expected integer, actual: bool
                error: expected integer, actual: bool
                error: expected integer, actual: bool
                error: expected integer, actual: bool
                error: expected integer, actual: bool
                error: expected integer, actual: bool
                error: expected integer, actual: bool
                error: expected integer, actual: bool
                error: expected integer, actual: bool
                error: expected integer, actual: string
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
                    10 + "aaa" + 10 + "aaa";
                    // 10 + "aaa" + 10 + "aaa": integer
                }
                ---
                error: expected integer, actual: string
                error: expected integer, actual: string
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
                    let a = -10
                    // -10: integer
                    let b = -"aaa"
                    // -"aaa": integer
                    let c = -'a'
                    // -'a': integer
                    let d = -true
                    // -true: integer
                    let e = !10
                    // !10: bool
                    let f = !"aaa"
                    // !"aaa": bool
                    let g = !'a'
                    // !'a': bool
                    let h = !true
                    // !true: bool
                }
                ---
                error: expected integer, actual: string
                error: expected integer, actual: char
                error: expected integer, actual: bool
                error: expected bool, actual: integer
                error: expected bool, actual: string
                error: expected bool, actual: char
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
                    let a = !true
                    // !true: bool
                    let b = !false
                    // !false: bool
                    expr:!a == !b
                    // !a == !b: bool
                }
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
                    let a = -10
                    // -10: integer
                    expr:a
                    // a: integer
                }
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
                        expr:10
                        // 10: integer
                    };
                    // { .. }: integer
                }
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
                            10
                            // 10: integer
                            expr:"aaa"
                            // "aaa": string
                        }
                        // { .. }: string
                    };
                    // { .. }: string
                }
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
                    let a = 10
                    // 10: integer
                    let b = {
                        let c = 20
                        // 20: integer
                        expr:a + c
                        // a + c: integer
                    }
                    // { .. }: integer
                    expr:b
                    // b: integer
                }
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
                    expr:10
                    // 10: integer
                }
                ---
                error: expected integer, actual: ()
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
                    10;
                    // 10: integer
                }
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
                        expr:10
                        // 10: integer
                    };
                    // { .. }: integer
                    {
                        20;
                        // 20: integer
                    };
                    // { .. }: ()
                }
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
                            expr:10
                            // 10: integer
                        }
                        // { .. }: integer
                    }
                    // { .. }: integer
                }
                ---
                error: expected integer, actual: ()
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
                            10;
                            // 10: integer
                        }
                        // { .. }: ()
                    }
                    // { .. }: ()
                }
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
                            expr:10
                            // 10: integer
                        };
                        // { .. }: integer
                    }
                    // { .. }: ()
                }
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
                    let a = 10
                    // 10: integer
                    expr:a
                    // a: integer
                }
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
                    let a = param:x
                    // param:x: integer
                    let b = param:y
                    // param:y: string
                }
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
                fn() -> ()
                fn(bool, string) -> int
                ---
                `true`: bool
                `"aaa"`: string
                `aaa(true, "aaa")`: int
                `res`: int
                `30`: int
                `res + 30`: int
                `10`: int
                `20`: int
                `10 + 20`: int
                ---
                error: expected (), found int by `res + 30`
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
                fn() -> ()
                fn(bool, string) -> int
                ---
                `"aaa"`: string
                `true`: bool
                `aaa("aaa", true)`: int
                `10`: int
                `20`: int
                `10 + 20`: int
                ---
                error: expected bool, found string by `"aaa"`
                error: expected string, found bool by `true`
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
                fn() -> ()
                ---
                `true`: bool
                `10`: int
                `{ .., 10 }`: int
                `20`: int
                `{ .., 20 }`: int
                `if true { .., 10 } else { .., 20 }`: int
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
                fn() -> int
                ---
                `true`: bool
                `10`: int
                `{ .., 10 }`: int
                `if true { .., 10 }`: unknown
                ---
                error: expected (), found int by `{ .., 10 }`
                error: expected int, found unknown by `if true { .., 10 }`
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
                fn() -> ()
                ---
                `true`: bool
                `{{ .. }}`: ()
                `{{ .. }}`: ()
                `if true {{ .. }} else {{ .. }}`: ()
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
                fn() -> ()
                ---
                `true`: bool
                `{{ .. }}`: ()
                `if true {{ .. }}`: ()
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
                fn() -> ()
                ---
                `true`: bool
                `10`: int
                `{ .., 10 }`: int
                `"aaa"`: string
                `{ .., "aaa" }`: string
                `if true { .., 10 } else { .., "aaa" }`: unknown
                ---
                error: expected int, found string by `{ .., 10 }` and `{ .., "aaa" }`
                error: expected (), found unknown by `if true { .., 10 } else { .., "aaa" }`
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
                fn() -> ()
                ---
                `10`: int
                `"aaa"`: string
                `{ .., "aaa" }`: string
                `"aaa"`: string
                `{ .., "aaa" }`: string
                `if 10 { .., "aaa" } else { .., "aaa" }`: string
                ---
                error: expected bool, found int by `10`
                error: expected (), found string by `if 10 { .., "aaa" } else { .., "aaa" }`
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
                fn() -> int
                ---
                `true`: bool
                `10`: int
                `return 10`: !
                `{{ .. }}`: ()
                `true`: bool
                `{ .., true }`: bool
                `if true {{ .. }} else { .., true }`: unknown
                `20`: int
                ---
                error: expected (), found bool by `{{ .. }}` and `{ .., true }`
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
                fn() -> int
                ---
                `true`: bool
                `true`: bool
                `{ .., true }`: bool
                `10`: int
                `return 10`: !
                `{{ .. }}`: ()
                `if true { .., true } else {{ .. }}`: unknown
                `20`: int
                ---
                error: expected bool, found () by `{ .., true }` and `{{ .. }}`
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
                fn() -> int
                ---
                `true`: bool
                `10`: int
                `return 10`: !
                `{{ .. }}`: ()
                `20`: int
                `return 20`: !
                `{{ .. }}`: ()
                `if true {{ .. }} else {{ .. }}`: ()
                `30`: int
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
                fn() -> ()
                ---
                `return`: !
                ---
                error: expected (), found ! by `return`
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    return 10
                }
            "#,
            expect![[r#"
                fn() -> int
                ---
                `10`: int
                `return 10`: !
                ---
                error: expected int, found ! by `return 10`
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
                fn() -> int
                ---
                `return`: !
                ---
                error: expected int, found ()
                error: expected int, found ! by `return`
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() -> int {
                    return "aaa"
                }
            "#,
            expect![[r#"
                fn() -> int
                ---
                `"aaa"`: string
                `return "aaa"`: !
                ---
                error: expected int, found string by `"aaa"`
                error: expected int, found ! by `return "aaa"`
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
                fn() -> ()
                fn() -> bool
                fn() -> string
                fn() -> int
                ---
                `return`: !
                `true`: bool
                `"aaa"`: string
                `30`: int
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

                fn main() -> integer {
                    mod_aaa::fn_aaa();
                    mod_aaa::mod_bbb::fn_bbb()
                }

                //- /mod_aaa.nail
                mod mod_bbb;
                fn fn_aaa() -> integer {
                    mod_bbb::fn_bbb()
                }

                //- /mod_aaa/mod_bbb.nail
                fn fn_bbb() -> integer {
                    10
                }
            "#,
            expect![[r#"
                fn() -> unknown
                fn() -> unknown
                fn() -> unknown
                ---
                `mod_aaa::fn_aaa()`: unknown
                `mod_aaa::mod_bbb::fn_bbb()`: unknown
                ---
            "#]],
        );
    }
}
