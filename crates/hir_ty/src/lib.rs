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

        let mut indexes = inference_result.type_by_exprs.keys().collect::<Vec<_>>();
        indexes.sort_by_cached_key(|idx| idx.into_raw());
        for idx in indexes {
            let expr = debug_hir_expr(idx, lower_result);
            msg.push_str(&format!(
                "`{}`: {}\n",
                expr,
                debug_type(&inference_result.type_by_exprs[idx])
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
            }
        }

        msg
    }

    fn debug_hir_expr(expr_idx: &hir::ExprIdx, lower_result: &hir::LowerResult) -> String {
        let expr = &lower_result.shared_ctx.exprs[*expr_idx];
        match expr {
            hir::Expr::Missing => "<missing>".to_string(),
            hir::Expr::Unary { op, expr } => {
                let op = match op {
                    hir::UnaryOp::Neg => "-".to_string(),
                };
                let expr = debug_hir_expr(expr, lower_result);
                format!("{op}{expr}")
            }
            hir::Expr::Binary { op, lhs, rhs } => {
                let op = match op {
                    hir::BinaryOp::Add => "+",
                    hir::BinaryOp::Sub => "+",
                    hir::BinaryOp::Mul => "+",
                    hir::BinaryOp::Div => "+",
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
                    .map(|idx| debug_hir_expr(idx, lower_result))
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
            ResolvedType::Function(_) => "fn",
        }
        .to_string()
    }

    #[test]
    fn infer_integer_literal() {
        check(
            r#"
                fn main() {
                    10
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
                    "aaa"
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
                    'a'
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
                    true
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
                    false
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
                    true + true
                    10 + true
                    10 + (10 + "aaa")
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
                `true`: bool
                `true`: bool
                `true + true`: unknown
                `10`: int
                `true`: bool
                `10 + true`: unknown
                `10`: int
                `"aaa"`: string
                `10`: int
                `10 + "aaa"`: int
                `10 + 10 + "aaa"`: int
                ---
                error: expected int, found string by `10` and `"aaa"`
                error: expected int, found char by `10` and `'a'`
                error: expected int, found bool by `10` and `true`
            "#]],
        );

        check(
            r#"
                fn main() {
                    (10 + "aaa") + (10 + "aaa")
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
                    let a = -10
                    let b = -"aaa"
                    let c = -'a'
                    let d = -true
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
                ---
            "#]],
        )
    }

    #[test]
    fn infer_variable_ref() {
        check(
            r#"
                fn main() {
                    let a = -10
                    a
                }
            "#,
            expect![[r#"
                fn() -> ()
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
                    }
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
                    }
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
                fn main() {
                    let a = 10;
                    let b = {
                        let c = 20;
                        a + c
                    }
                    b
                }
            "#,
            expect![[r#"
                fn() -> ()
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
    fn infer_function() {
        check(
            r#"
                fn aaa() {
                    let a = 10
                    a
                }
            "#,
            expect![[r#"
                fn() -> ()
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
}
