mod checker;
mod inference;

use hir::LowerResult;

pub use checker::{TypeCheckError, TypeCheckResult};
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

    use crate::inference::infer;

    use super::*;

    fn check(input: &str, expect: Expect) {
        let parsed = parser::parse(input);
        let ast = ast::SourceFile::cast(parsed.syntax()).unwrap();
        let lower_result = hir::lower(ast);
        let result = infer(&lower_result);
        expect.assert_eq(&debug(&result, &lower_result));
    }

    fn debug(result: &InferenceResult, lower_result: &hir::LowerResult) -> String {
        let mut msg = "".to_string();

        for (_, signature) in result.signatures.iter() {
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

        let mut indexes = result.type_by_exprs.keys().collect::<Vec<_>>();
        indexes.sort_by_cached_key(|idx| idx.into_raw());
        for idx in indexes {
            let expr = debug_hir_expr(idx, lower_result);
            msg.push_str(&format!(
                "`{}`: {}\n",
                expr,
                debug_type(&result.type_by_exprs[idx])
            ));
        }

        msg
    }

    fn debug_hir_expr(expr_idx: &hir::ExprIdx, lower_result: &hir::LowerResult) -> String {
        let expr = &lower_result.shared_ctx.exprs[*expr_idx];
        match expr {
            hir::Expr::Missing => "".to_string(),
            hir::Expr::Unary { op, expr } => {
                let op = match op {
                    hir::UnaryOp::Neg => "-".to_string(),
                };
                let expr = debug_hir_expr(expr, lower_result);
                format!("{}{}", op, expr)
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

                format!("{} {} {}", lhs, op, rhs)
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

                format!("{}({})", name, args)
            }
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Bool(b) => b.to_string(),
                hir::Literal::Char(c) => format!("'{}'", c),
                hir::Literal::Integer(i) => i.to_string(),
                hir::Literal::String(s) => format!("\"{}\"", s),
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
            "10",
            expect![[r#"
                ---
                `10`: int
            "#]],
        );
    }

    #[test]
    fn infer_string_literal() {
        check(
            "\"aaa\"",
            expect![[r#"
                ---
                `"aaa"`: string
            "#]],
        );
    }

    #[test]
    fn infer_char_literal() {
        check(
            "'a'",
            expect![[r#"
                ---
                `'a'`: char
            "#]],
        );
    }

    #[test]
    fn infer_bool_test() {
        check(
            "true",
            expect![[r#"
                ---
                `true`: bool
            "#]],
        );

        check(
            "false",
            expect![[r#"
                ---
                `false`: bool
            "#]],
        );
    }

    #[test]
    fn infer_variable_def() {
        check(
            "let a = true",
            expect![[r#"
                ---
                `true`: bool
            "#]],
        )
    }

    #[test]
    fn infer_multiline_variable_def() {
        check(
            r#"
                let a = true
                let b = 10
                let c = "aa"
                let d = 'a'
            "#,
            expect![[r#"
                ---
                `true`: bool
                `10`: int
                `"aa"`: string
                `'a'`: char
            "#]],
        )
    }

    #[test]
    fn infer_binary() {
        check(
            r#"
                10 + 20
                "aaa" + "bbb"
                10 + "aaa"
                'a' + 'a'
                10 + 'a'
                true + true
                10 + true
                10 + (10 + "aaa")
            "#,
            expect![[r#"
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
            "#]],
        );

        check(
            r#"
                (10 + "aaa") + (10 + "aaa")
            "#,
            expect![[r#"
                ---
                `10`: int
                `"aaa"`: string
                `10`: int
                `"aaa"`: string
                `10 + "aaa"`: unknown
                `10 + "aaa"`: unknown
                `10 + "aaa" + 10 + "aaa"`: unknown
            "#]],
        );
    }

    #[test]
    fn infer_unary() {
        check(
            r#"
                let a = -10
                let b = -"aaa"
                let c = -'a'
                let d = -true
            "#,
            expect![[r#"
                ---
                `10`: int
                `-10`: int
                `"aaa"`: string
                `-"aaa"`: unknown
                `'a'`: char
                `-'a'`: unknown
                `true`: bool
                `-true`: unknown
            "#]],
        )
    }

    #[test]
    fn infer_variable_ref() {
        check(
            r#"
                let a = -10
                a
            "#,
            expect![[r#"
                ---
                `10`: int
                `-10`: int
                `a`: int
            "#]],
        )
    }

    #[test]
    fn infer_block() {
        check(
            r#"
                {
                    10
                }
            "#,
            expect![[r#"
                ---
                `10`: int
                `{ .., 10 }`: int
            "#]],
        );

        check(
            r#"
                {
                    {
                        10
                        "aaa"
                    }
                }
            "#,
            expect![[r#"
                ---
                `10`: int
                `"aaa"`: string
                `{ .., "aaa" }`: string
                `{ .., { .., "aaa" } }`: string
            "#]],
        );

        check(
            r#"
                let a = 10;
                let b = {
                    let c = 20;
                    a + c
                }
                b
            "#,
            expect![[r#"
                ---
                `10`: int
                `20`: int
                `a`: int
                `c`: int
                `a + c`: int
                `{ .., a + c }`: int
                `b`: int
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
            "#]],
        );
    }

    #[test]
    fn infer_call() {
        check(
            r#"
                fn aaa(x: bool, y: string) -> int {
                    10 + 20
                }
                let res = aaa(true, "aaa");
                res + 30
            "#,
            expect![[r#"
                fn(bool, string) -> int
                ---
                `10`: int
                `20`: int
                `10 + 20`: int
                `true`: bool
                `aaa(true)`: int
                `res`: int
                `30`: int
                `res + 30`: int
            "#]],
        );
    }
}
