mod checker;
mod inference;

pub use checker::{TypeCheckError, TypeCheckResult};
use hir::LowerResult;
pub use inference::{InferenceError, InferenceResult, ResolvedType, Signature};

pub fn lower(lower_result: &LowerResult) -> TyLowerResult {
    let inference_result = inference::infer(lower_result);
    let type_check_result = checker::check_type(lower_result, &inference_result);

    TyLowerResult {
        inference_result,
        type_check_result,
    }
}

pub struct TyLowerResult {
    pub inference_result: InferenceResult,
    pub type_check_result: TypeCheckResult,
}
impl TyLowerResult {
    pub fn signature_by_function(&self, function_id: hir::FunctionId) -> &Signature {
        let signature_idx = self.inference_result.signature_by_function[&function_id];
        &self.inference_result.signatures[signature_idx]
    }

    pub fn type_by_param(&self, param_id: hir::ParamId) -> ResolvedType {
        self.inference_result.type_by_param[&param_id]
    }

    pub fn type_by_expr(&self, expr: hir::ExprId) -> ResolvedType {
        self.inference_result.type_by_expr[&expr]
    }
}

#[cfg(test)]
mod tests {
    use ast::AstNode;
    use expect_test::{expect, Expect};

    use super::*;

    fn check(input: &str, expect: Expect) {
        let parsed = parser::parse(input);
        let ast = ast::SourceFile::cast(parsed.syntax()).unwrap();
        let lower_result = hir::lower(ast);
        let result = lower(&lower_result);
        expect.assert_eq(&debug(
            &result.inference_result,
            &result.type_check_result,
            &lower_result,
        ));
    }

    fn debug(
        inference_result: &InferenceResult,
        check_result: &TypeCheckResult,
        lower_result: &hir::LowerResult,
    ) -> String {
        let mut msg = "".to_string();

        for (_, signature) in inference_result.signatures.iter() {
            let params = signature
                .params
                .iter()
                .map(debug_type)
                .collect::<Vec<String>>()
                .join(", ");
            msg.push_str(&format!(
                "fn({}) -> {}\n",
                params,
                debug_type(&signature.return_type)
            ));
        }

        msg.push_str("---\n");

        let mut indexes = inference_result.type_by_expr.keys().collect::<Vec<_>>();
        indexes.sort();
        for expr_id in indexes {
            let expr = debug_hir_expr(expr_id, lower_result);
            msg.push_str(&format!(
                "`{}`: {}\n",
                expr,
                debug_type(&inference_result.type_by_expr[expr_id])
            ));
        }

        msg.push_str("---\n");

        for errors in &check_result.errors {
            match errors {
                TypeCheckError::UnresolvedType { expr } => {
                    msg.push_str(&format!(
                        "error: `{}` is unresolved type.\n",
                        debug_hir_expr(expr, lower_result),
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
                        debug_hir_expr(expected_expr, lower_result),
                        debug_hir_expr(found_expr, lower_result)
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
                    debug_hir_expr(found_expr, lower_result)
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
                        debug_hir_expr(found_expr, lower_result)
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
                        debug_hir_expr(found_expr, lower_result)
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
                            debug_hir_expr(found_expr, lower_result)
                        ));
                    }
                    msg.push('\n');
                }
            }
        }

        msg
    }

    fn debug_hir_expr(expr_id: &hir::ExprId, lower_result: &hir::LowerResult) -> String {
        let expr = expr_id.lookup(&lower_result.shared_ctx);
        match expr {
            hir::Expr::Missing => "<missing>".to_string(),
            hir::Expr::Unary { op, expr } => {
                let op = match op {
                    ast::UnaryOp::Neg(_) => "-".to_string(),
                    ast::UnaryOp::Not(_) => "!".to_string(),
                };
                let expr = debug_hir_expr(expr, lower_result);
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
                let lhs = debug_hir_expr(lhs, lower_result);
                let rhs = debug_hir_expr(rhs, lower_result);

                format!("{lhs} {op} {rhs}")
            }
            hir::Expr::VariableRef { var } => {
                let name = match var {
                    hir::Symbol::Param { name, .. } => name,
                    hir::Symbol::Local { name, .. } => name,
                    hir::Symbol::Function { name, .. } => name,
                    hir::Symbol::Missing { name, .. } => name,
                };
                lower_result.interner.lookup(name.key()).to_string()
            }
            hir::Expr::Block(block) => {
                if let Some(tail) = block.tail {
                    format!("{{ .., {} }}", debug_hir_expr(&tail, lower_result))
                } else {
                    "{{ .. }}".to_string()
                }
            }
            hir::Expr::Call { callee, args } => {
                let name = match callee {
                    hir::Symbol::Param { name, .. } => name,
                    hir::Symbol::Local { name, .. } => name,
                    hir::Symbol::Function { name, .. } => name,
                    hir::Symbol::Missing { name, .. } => name,
                };
                let name = lower_result.interner.lookup(name.key()).to_string();
                let args = args
                    .iter()
                    .map(|id| debug_hir_expr(id, lower_result))
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
                    debug_hir_expr(condition, lower_result),
                    debug_hir_expr(then_branch, lower_result)
                );
                if let Some(else_branch) = else_branch {
                    if_expr.push_str(&format!(
                        " else {}",
                        debug_hir_expr(else_branch, lower_result)
                    ));
                }

                if_expr
            }
            hir::Expr::Return { value } => {
                let mut msg = "return".to_string();
                if let Some(value) = value {
                    msg.push_str(&format!(" {}", debug_hir_expr(value, lower_result)));
                }

                msg
            }
        }
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
        check(
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
        check(
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
        check(
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
        check(
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
        check(
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

        check(
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
        check(
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
        check(
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
        check(
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

        check(
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
        check(
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
        check(
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
        check(
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
        check(
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

        check(
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

        check(
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
        check(
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

        check(
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

        check(
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
        check(
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

        check(
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

        check(
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
        check(
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
        check(
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
        check(
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
                `10`: int
                `20`: int
                `10 + 20`: int
                `true`: bool
                `"aaa"`: string
                `aaa(true, "aaa")`: int
                `res`: int
                `30`: int
                `res + 30`: int
                ---
                error: expected (), found int by `res + 30`
            "#]],
        );
    }

    #[test]
    fn infer_call_missmatch() {
        check(
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
                `10`: int
                `20`: int
                `10 + 20`: int
                `"aaa"`: string
                `true`: bool
                `aaa("aaa", true)`: int
                ---
                error: expected bool, found string by `"aaa"`
                error: expected string, found bool by `true`
            "#]],
        );
    }

    #[test]
    fn infer_if_expr() {
        check(
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
        check(
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
        check(
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

        check(
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
        check(
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
        check(
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
        check(
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

        check(
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

        check(
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
        check(
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

        check(
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
        check(
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

        check(
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
}
