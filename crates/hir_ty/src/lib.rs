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
pub use inference::{
    InferenceBodyResult, InferenceError, InferenceResult, ResolvedType, Signature,
};

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
        let signature_idx = self.inference_result.signature_by_function[&function_id];
        &self.inference_result.signatures[signature_idx]
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};
    use hir::{Name, Path, Symbol, TestingDatabase};

    use super::*;

    fn check_pod_start_with_root_file(fixture: &str, expect: Expect) {
        let db = TestingDatabase::default();
        let mut source_db = hir::FixtureDatabase::new(&db, fixture);

        let pods = hir::parse_pods(&db, "/main.nail", &mut source_db);
        let ty_result = lower_pods(&db, &pods);

        expect.assert_eq(&debug_file(&db, &ty_result, &pods.root_pod.root_hir_file));
    }

    fn check_in_root_file(fixture: &str, expect: Expect) {
        let mut fixture = fixture.to_string();
        fixture.insert_str(0, "//- /main.nail\n");

        check_pod_start_with_root_file(&fixture, expect);
    }

    fn debug_file(
        db: &dyn hir::HirMasterDatabase,
        ty_lower_result: &TyLowerResult,
        hir_file: &hir::HirFile,
    ) -> String {
        let mut msg = "".to_string();

        let TyLowerResult {
            type_check_result,
            inference_result,
        } = ty_lower_result;

        for (_, signature) in inference_result.signatures.iter() {
            let params = signature
                .params
                .iter()
                .map(debug_type)
                .collect::<Vec<String>>()
                .join(", ");
            msg.push_str(&format!(
                "fn({params}) -> {}\n",
                debug_type(&signature.return_type)
            ));
        }

        msg.push_str("---\n");

        for function in hir_file.functions(db) {
            let inference_body_result = inference_result.inference_by_body.get(function).unwrap();

            let mut indexes = inference_body_result
                .type_by_expr
                .keys()
                .collect::<Vec<_>>();
            indexes.sort();
            for expr_id in indexes {
                let expr = debug_hir_expr(db, expr_id, hir_file);
                msg.push_str(&format!(
                    "`{}`: {}\n",
                    expr,
                    debug_type(&inference_body_result.type_by_expr[expr_id])
                ));
            }
        }

        msg.push_str("---\n");

        for function in hir_file.functions(db) {
            let type_check_errors = type_check_result.errors_by_function.get(function).unwrap();

            for error in type_check_errors {
                match error {
                    TypeCheckError::UnresolvedType { expr } => {
                        msg.push_str(&format!(
                            "error: `{}` is unresolved type.\n",
                            debug_hir_expr(db, expr, hir_file),
                        ));
                    }
                    TypeCheckError::MismatchedTypes {
                        expected_expr,
                        expected_ty,
                        found_expr,
                        found_ty,
                    } => {
                        msg.push_str(&format!(
                            "error: expected {}, found {} by `{}` and `{}`\n",
                            debug_type(expected_ty),
                            debug_type(found_ty),
                            debug_hir_expr(db, expected_expr, hir_file),
                            debug_hir_expr(db, found_expr, hir_file)
                        ));
                    }
                    TypeCheckError::MismaatchedSignature {
                        expected_ty,
                        found_expr,
                        found_ty,
                        ..
                    } => msg.push_str(&format!(
                        "error: expected {}, found {} by `{}`\n",
                        debug_type(expected_ty),
                        debug_type(found_ty),
                        debug_hir_expr(db, found_expr, hir_file)
                    )),
                    TypeCheckError::MismatchedTypeIfCondition {
                        expected_ty,
                        found_expr,
                        found_ty,
                    } => {
                        msg.push_str(&format!(
                            "error: expected {}, found {} by `{}`\n",
                            debug_type(expected_ty),
                            debug_type(found_ty),
                            debug_hir_expr(db, found_expr, hir_file)
                        ));
                    }
                    TypeCheckError::MismatchedTypeElseBranch {
                        expected_ty,
                        found_expr,
                        found_ty,
                    } => {
                        msg.push_str(&format!(
                            "error: expected {}, found {} by `{}`\n",
                            debug_type(expected_ty),
                            debug_type(found_ty),
                            debug_hir_expr(db, found_expr, hir_file)
                        ));
                    }
                    TypeCheckError::MismatchedReturnType {
                        expected_ty,
                        found_expr,
                        found_ty,
                    } => {
                        msg.push_str(&format!(
                            "error: expected {}, found {}",
                            debug_type(expected_ty),
                            debug_type(found_ty)
                        ));
                        if let Some(found_expr) = found_expr {
                            msg.push_str(&format!(
                                " by `{}`",
                                debug_hir_expr(db, found_expr, hir_file)
                            ));
                        }
                        msg.push('\n');
                    }
                }
            }
        }

        msg
    }

    fn debug_hir_expr(
        db: &dyn hir::HirMasterDatabase,
        expr_id: &hir::ExprId,
        hir_file: &hir::HirFile,
    ) -> String {
        let expr = expr_id.lookup(hir_file.db(db));
        match expr {
            hir::Expr::Symbol(symbol) => match symbol {
                hir::Symbol::Param { name, .. } => debug_name(db, *name),
                hir::Symbol::Local { name, .. } => debug_name(db, *name),
                hir::Symbol::Missing { path, .. } => debug_path(db, &path.path(db)),
            },
            hir::Expr::Missing => "<missing>".to_string(),
            hir::Expr::Unary { op, expr } => {
                let op = match op {
                    ast::UnaryOp::Neg(_) => "-".to_string(),
                    ast::UnaryOp::Not(_) => "!".to_string(),
                };
                let expr = debug_hir_expr(db, expr, hir_file);
                format!("{op}{expr}")
            }
            hir::Expr::Binary { op, lhs, rhs } => {
                let op = match op {
                    ast::BinaryOp::Add(_) => "+",
                    ast::BinaryOp::Sub(_) => "-",
                    ast::BinaryOp::Mul(_) => "*",
                    ast::BinaryOp::Div(_) => "/",
                    ast::BinaryOp::Equal(_) => "==",
                    ast::BinaryOp::GreaterThan(_) => ">",
                    ast::BinaryOp::LessThan(_) => "<",
                }
                .to_string();
                let lhs = debug_hir_expr(db, lhs, hir_file);
                let rhs = debug_hir_expr(db, rhs, hir_file);

                format!("{lhs} {op} {rhs}")
            }
            hir::Expr::Block(block) => {
                if let Some(tail) = block.tail {
                    format!("{{ .., {} }}", debug_hir_expr(db, &tail, hir_file))
                } else {
                    "{{ .. }}".to_string()
                }
            }
            hir::Expr::Call { callee, args } => {
                let name = debug_symbol(db, callee);
                let args = args
                    .iter()
                    .map(|id| debug_hir_expr(db, id, hir_file))
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("{name}({args})")
            }
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Bool(b) => b.to_string(),
                hir::Literal::Char(c) => format!("'{c}'"),
                hir::Literal::Integer(i) => i.to_string(),
                hir::Literal::String(s) => format!("\"{s}\""),
            },
            hir::Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut if_expr = format!(
                    "if {} {}",
                    debug_hir_expr(db, condition, hir_file),
                    debug_hir_expr(db, then_branch, hir_file)
                );
                if let Some(else_branch) = else_branch {
                    if_expr.push_str(&format!(
                        " else {}",
                        debug_hir_expr(db, else_branch, hir_file)
                    ));
                }

                if_expr
            }
            hir::Expr::Return { value } => {
                let mut msg = "return".to_string();
                if let Some(value) = value {
                    msg.push_str(&format!(" {}", debug_hir_expr(db, value, hir_file)));
                }

                msg
            }
        }
    }

    fn debug_symbol(db: &dyn hir::HirMasterDatabase, symbol: &Symbol) -> String {
        match symbol {
            hir::Symbol::Param { name, .. } => debug_name(db, *name),
            hir::Symbol::Local { name, .. } => debug_name(db, *name),
            hir::Symbol::Missing { path, .. } => debug_path(db, &path.path(db)),
        }
    }

    fn debug_name(db: &dyn hir::HirMasterDatabase, name: Name) -> String {
        name.text(db).to_string()
    }

    fn debug_path(db: &dyn hir::HirMasterDatabase, path: &Path) -> String {
        path.segments(db)
            .iter()
            .map(|segment| segment.text(db).to_string())
            .collect::<Vec<_>>()
            .join("::")
    }

    fn debug_type(ty: &ResolvedType) -> String {
        match ty {
            ResolvedType::Unknown => "unknown",
            ResolvedType::Integer => "int",
            ResolvedType::String => "string",
            ResolvedType::Char => "char",
            ResolvedType::Bool => "bool",
            ResolvedType::Unit => "()",
            ResolvedType::Never => "!",
            ResolvedType::Function(_) => "fn",
        }
        .to_string()
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
                fn() -> ()
                ---
                `10`: int
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
                fn() -> ()
                ---
                `"aaa"`: string
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
                fn() -> ()
                ---
                `'a'`: char
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
                fn() -> ()
                ---
                `true`: bool
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
                fn() -> ()
                ---
                `false`: bool
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
                fn() -> ()
                ---
                `true`: bool
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
                fn() -> ()
                ---
                `true`: bool
                `10`: int
                `"aa"`: string
                `'a'`: char
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
                fn() -> ()
                ---
                `10`: int
                `20`: int
                `10 + 20`: int
                `"aaa"`: string
                `"bbb"`: string
                `"aaa" + "bbb"`: unknown
                `10`: int
                `"aaa"`: string
                `10 + "aaa"`: unknown
                `'a'`: char
                `'a'`: char
                `'a' + 'a'`: unknown
                `10`: int
                `'a'`: char
                `10 + 'a'`: unknown
                `10`: int
                `'a'`: char
                `10 < 'a'`: unknown
                `10`: int
                `'a'`: char
                `10 > 'a'`: unknown
                `true`: bool
                `true`: bool
                `true + true`: unknown
                `true`: bool
                `true`: bool
                `true - true`: unknown
                `true`: bool
                `true`: bool
                `true * true`: unknown
                `true`: bool
                `true`: bool
                `true / true`: unknown
                `true`: bool
                `true`: bool
                `true == true`: bool
                `true`: bool
                `false`: bool
                `true < false`: unknown
                `true`: bool
                `false`: bool
                `true > false`: unknown
                `10`: int
                `true`: bool
                `10 + true`: unknown
                `10`: int
                `"aaa"`: string
                `10`: int
                `10 + "aaa"`: int
                `10 + 10 + "aaa"`: int
                `10`: int
                `20`: int
                `10 - 20`: int
                `10`: int
                `20`: int
                `10 * 20`: int
                `10`: int
                `20`: int
                `10 / 20`: int
                `10`: int
                `20`: int
                `10 == 20`: bool
                `10`: int
                `20`: int
                `10 < 20`: bool
                `10`: int
                `20`: int
                `10 > 20`: bool
                ---
                error: expected int, found string by `10` and `"aaa"`
                error: expected int, found char by `10` and `'a'`
                error: expected int, found char by `10` and `'a'`
                error: expected int, found char by `10` and `'a'`
                error: expected int, found bool by `10` and `true`
            "#]],
        );

        check_in_root_file(
            r#"
                fn main() {
                    (10 + "aaa") + (10 + "aaa");
                }
            "#,
            expect![[r#"
                fn() -> ()
                ---
                `10`: int
                `"aaa"`: string
                `10`: int
                `"aaa"`: string
                `10 + "aaa"`: unknown
                `10 + "aaa"`: unknown
                `10 + "aaa" + 10 + "aaa"`: unknown
                ---
                error: `10 + "aaa"` is unresolved type.
                error: `10 + "aaa"` is unresolved type.
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
                fn() -> ()
                ---
                `10`: int
                `-10`: int
                `"aaa"`: string
                `-"aaa"`: unknown
                `'a'`: char
                `-'a'`: unknown
                `true`: bool
                `-true`: unknown
                `10`: int
                `!10`: unknown
                `"aaa"`: string
                `!"aaa"`: unknown
                `'a'`: char
                `!'a'`: unknown
                `true`: bool
                `!true`: bool
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
                fn() -> bool
                ---
                `true`: bool
                `!true`: bool
                `false`: bool
                `!false`: bool
                `a`: bool
                `b`: bool
                `!a`: bool
                `!b`: bool
                `!a == !b`: bool
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
                fn() -> int
                ---
                `10`: int
                `-10`: int
                `a`: int
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
                fn() -> ()
                ---
                `10`: int
                `{ .., 10 }`: int
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
                fn() -> ()
                ---
                `10`: int
                `"aaa"`: string
                `{ .., "aaa" }`: string
                `{ .., { .., "aaa" } }`: string
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
                fn() -> int
                ---
                `10`: int
                `20`: int
                `a`: int
                `c`: int
                `a + c`: int
                `{ .., a + c }`: int
                `b`: int
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
                fn() -> ()
                ---
                `10`: int
                ---
                error: expected (), found int by `10`
            "#]],
        );

        check_in_root_file(
            r#"
                fn aaa() {
                    10;
                }
            "#,
            expect![[r#"
                fn() -> ()
                ---
                `10`: int
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
                fn() -> ()
                ---
                `10`: int
                `{ .., 10 }`: int
                `20`: int
                `{{ .. }}`: ()
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
                fn() -> ()
                ---
                `10`: int
                `{ .., 10 }`: int
                `{ .., { .., 10 } }`: int
                ---
                error: expected (), found int by `{ .., { .., 10 } }`
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
                fn() -> ()
                ---
                `10`: int
                `{{ .. }}`: ()
                `{ .., {{ .. }} }`: ()
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
                fn() -> ()
                ---
                `10`: int
                `{ .., 10 }`: int
                `{{ .. }}`: ()
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
                fn() -> int
                ---
                `10`: int
                `a`: int
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
                fn(int, string) -> ()
                ---
                `x`: int
                `y`: string
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
