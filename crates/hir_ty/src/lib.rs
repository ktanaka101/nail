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
            msg.push('\n');

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
                    "{indent}expr:{}",
                    self.debug_expr(hir_file, function, scope_origin, tail, nesting + 1)
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
                    let expr_str =
                        self.debug_expr(hir_file, function, scope_origin, *expr, nesting);
                    let type_line = self.debug_type_line(function, *expr);
                    let maybe_semicolon = if *has_semicolon { ";" } else { "" };
                    format!("{indent}{expr_str}{maybe_semicolon} {type_line}\n")
                }
                hir::Stmt::Item { item } => self.debug_item(hir_file, *item, nesting),
            }
        }

        fn debug_type_line(&self, function: hir::Function, expr_id: hir::ExprId) -> String {
            let ty = self
                .inference_result
                .inference_body_result_by_function
                .get(&function)
                .unwrap()
                .type_by_expr
                .get(&expr_id)
                .unwrap();

            format!("//: {ty}")
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
                            "{indent}expr:{}",
                            self.debug_expr(hir_file, function, scope_origin, tail, nesting + 1)
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
                }

                ---
                error: expected int, actual: string
                error: expected int, actual: string
                error: expected int, actual: string
                error: expected int, actual: char
                error: expected int, actual: char
                error: expected int, actual: char
                error: expected int, actual: char
                error: expected int, actual: char
                error: expected int, actual: bool
                error: expected int, actual: bool
                error: expected int, actual: bool
                error: expected int, actual: bool
                error: expected int, actual: bool
                error: expected int, actual: bool
                error: expected int, actual: bool
                error: expected int, actual: bool
                error: expected int, actual: bool
                error: expected int, actual: string
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
                error: expected int, actual: string
                error: expected int, actual: string
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
                error: expected int, actual: string
                error: expected int, actual: char
                error: expected int, actual: bool
                error: expected bool, actual: int
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
                    let a = !true; //: bool
                    let b = !false; //: bool
                    expr:!a == !b //: bool
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
                    let a = -10; //: int
                    expr:a //: int
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
                        expr:10 //: int
                    }; //: int
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
                            10 //: int
                            expr:"aaa" //: string
                        } //: string
                    }; //: string
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
                    let a = 10; //: int
                    let b = {
                        let c = 20; //: int
                        expr:a + c //: int
                    }; //: int
                    expr:b //: int
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
                    expr:10 //: int
                }

                ---
                error: expected int, actual: ()
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
                error: expected int, actual: ()
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
                error: expected int, actual: ()
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
                error: expected string, actual: bool
                error: expected bool, actual: string
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
                error: expected int, actual: ()
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
                error: expected int, actual: string
                error: expected int, actual: ()
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
                error: expected bool, actual: int
                error: expected string, actual: ()
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
                    }; //: ()
                    expr:20 //: int
                }

                ---
                error: expected (), actual: bool
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
                error: expected bool, actual: ()
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
                    }; //: ()
                    expr:30 //: int
                }

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
                error: expected !, actual: ()
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
                error: expected !, actual: int
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
                error: expected (), actual: int
                error: expected !, actual: int
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
                error: expected string, actual: int
                error: expected !, actual: int
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
            "#]],
        );
    }
}
