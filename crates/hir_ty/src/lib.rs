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
pub mod testing;

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
    pub fn type_inference_errors_with_function(&self) -> Vec<(hir::Function, &InferenceError)> {
        self.inference_result
            .inference_body_result_by_function
            .iter()
            .flat_map(|(function, inference_body_result)| {
                inference_body_result
                    .errors
                    .iter()
                    .map(|error| (*function, error))
            })
            .collect()
    }

    /// 型チェックエラーを取得します。
    pub fn type_check_errors_with_function(&self) -> Vec<(hir::Function, &TypeCheckError)> {
        self.type_check_result
            .errors_by_function
            .iter()
            .flat_map(|(function, type_check_error)| {
                type_check_error.iter().map(|error| (*function, error))
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{lower_pods, testing::TestingDatabase};

    fn check_pod_start_with_root_file(fixture: &str, expect: Expect) {
        let db = TestingDatabase::default();
        let source_db = hir::FixtureDatabase::new(&db, fixture);

        let pods = hir::parse_pods(&db, &source_db);
        let ty_lower_result = lower_pods(&db, &pods);

        expect.assert_eq(&crate::testing::Pretty::new(&db, &pods, &ty_lower_result).format());
    }

    fn check_in_root_file(fixture: &str, expect: Expect) {
        let mut fixture = fixture.to_string();
        fixture.insert_str(0, "//- /main.nail\n");

        check_pod_start_with_root_file(&fixture, expect);
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
    fn infer_let() {
        check_in_root_file(
            r#"
                fn main() {
                    let a = true;
                    let mut b = 10;
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    let a = true; //: bool
                    let mut b = 10; //: int
                }

                ---
                ---
            "#]],
        )
    }

    #[test]
    fn infer_multiline_let() {
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
                    10 > 'a'
                    10 < 'a'
                    10 >= 'a'
                    10 <= 'a'
                    true + true
                    true - true
                    true * true
                    true / true
                    true == true
                    true != true
                    true < false
                    true > false
                    10 + true
                    10 + (10 + "aaa")
                    10 - 20
                    10 * 20
                    10 / 20
                    10 == 20
                    10 > 20
                    10 < 20;
                    10 >= 20
                    10 <= 20;
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
                    10 > 'a' //: bool
                    10 < 'a' //: bool
                    10 >= 'a' //: bool
                    10 <= 'a' //: bool
                    true + true //: int
                    true - true //: int
                    true * true //: int
                    true / true //: int
                    true == true //: bool
                    true != true //: bool
                    true < false //: bool
                    true > false //: bool
                    10 + true //: int
                    10 + 10 + "aaa" //: int
                    10 - 20 //: int
                    10 * 20 //: int
                    10 / 20 //: int
                    10 == 20 //: bool
                    10 > 20 //: bool
                    10 < 20; //: bool
                    10 >= 20 //: bool
                    10 <= 20; //: bool
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
                error MismatchedBinaryCompare: op: >, expected_ty: int, found_ty: char, expected_expr: `10`, found_expr: `'a'`
                error MismatchedBinaryCompare: op: <, expected_ty: int, found_ty: char, expected_expr: `10`, found_expr: `'a'`
                error MismatchedBinaryCompare: op: >=, expected_ty: int, found_ty: char, expected_expr: `10`, found_expr: `'a'`
                error MismatchedBinaryCompare: op: <=, expected_ty: int, found_ty: char, expected_expr: `10`, found_expr: `'a'`
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
                error ImmutableReassignment: expr: a = 20
                error ImmutableReassignment: expr: a = "aaa"
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
    fn infer_not() {
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
                    let a = -10;
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
                fn aaa(x: int, y: string, z: mut bool) {
                    let a = x;
                    let b = y;
                    let c = z;
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn aaa(x: int, y: string, z: mut bool) -> () {
                    let a = param:x; //: int
                    let b = param:y; //: string
                    let c = param:z; //: bool
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
                    let mut i = 1;
                    while i < 3 {
                        i = i + 1;
                    }

                    i
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> int {
                    let mut i = 1; //: int
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

    #[test]
    fn check_reassignment_allow_mutable() {
        check_pod_start_with_root_file(
            r#"
                //- /main.nail
                fn main() {
                    let a = 10;
                    a = 20;

                    let mut b = 30;
                    b = 40;
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    let a = 10; //: int
                    a = 20; //: ()
                    let mut b = 30; //: int
                    b = 40; //: ()
                }

                ---
                ---
                error ImmutableReassignment: expr: a = 20
            "#]],
        );
    }

    #[test]
    fn check_multiple_reassignment_reference_latest_mutable() {
        check_pod_start_with_root_file(
            r#"
                //- /main.nail
                fn main() {
                    let a = 10;
                    a = 20;

                    let mut a = 40;
                    a = 50;

                    let a = 70;
                    a = 80;
                }
            "#,
            expect![[r#"
                //- /main.nail
                fn entry:main() -> () {
                    let a = 10; //: int
                    a = 20; //: ()
                    let mut a = 40; //: int
                    a = 50; //: ()
                    let a = 70; //: int
                    a = 80; //: ()
                }

                ---
                ---
                error ImmutableReassignment: expr: a = 20
                error ImmutableReassignment: expr: a = 80
            "#]],
        );
    }

    #[test]
    fn init_struct_unit() {
        check_in_root_file(
            r#"
                struct Point;
                fn main() {
                    let point = Point;
                }
            "#,
            expect![[r#"
                //- /main.nail
                struct Point;
                fn entry:main() -> () {
                    let point = struct:Point; //: Point
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn init_struct_unit_other_init() {
        check_in_root_file(
            r#"
                struct Point;
                fn main() {
                    Point();
                    Point { x: 10 };
                }
            "#,
            expect![[r#"
                //- /main.nail
                struct Point;
                fn entry:main() -> () {
                    struct:Point(); //: <unknown>
                    struct:Point { x: 10 }; //: <unknown>
                }

                ---
                error NotCallable: found_callee_ty: Point, found_callee_symbol: struct:Point
                error NotRecord: found_struct_ty: Point, found_struct_symbol: struct:Point, found_expr: `struct:Point { x: 10 }`
                ---
                error Type is unknown: expr: struct:Point()
                error Type is unknown: expr: struct:Point { x: 10 }
            "#]],
        );
    }

    #[test]
    fn init_struct_tuple_fileds() {
        check_in_root_file(
            r#"
                struct Point(int, int);
                fn main() {
                    let point = Point(10, 20);
                }
            "#,
            expect![[r#"
                //- /main.nail
                struct Point(int, int);
                fn entry:main() -> () {
                    let point = struct:Point(10, 20); //: Point
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn init_struct_tuple_fileds_diff_arg_count() {
        check_in_root_file(
            r#"
                struct Point(int, int);
                fn main() {
                    Point(10);
                    Point(10, 20, 30);
                }
            "#,
            expect![[r#"
                //- /main.nail
                struct Point(int, int);
                fn entry:main() -> () {
                    struct:Point(10); //: Point
                    struct:Point(10, 20, 30); //: Point
                }

                ---
                error MismatchedCallArgCount: expected_arg_count: 2, found_arg_count: 1
                error MismatchedCallArgCount: expected_arg_count: 2, found_arg_count: 3
                ---
            "#]],
        );
    }

    #[test]
    fn init_struct_tuple_fileds_other_init() {
        check_in_root_file(
            r#"
                struct Point(int, int);
                fn main() {
                    Point;
                    Point { x: 10, y: 20 };
                }
            "#,
            expect![[r#"
                //- /main.nail
                struct Point(int, int);
                fn entry:main() -> () {
                    struct:Point; //: <unknown>
                    struct:Point { x: 10, y: 20 }; //: <unknown>
                }

                ---
                error NeededInitTupleOrRecord: found_ty: Point, found_expr: `struct:Point`, found_struct: Point
                error NotRecord: found_struct_ty: Point, found_struct_symbol: struct:Point, found_expr: `struct:Point { x: 10, y: 20 }`
                ---
                error Type is unknown: expr: struct:Point
                error Type is unknown: expr: struct:Point { x: 10, y: 20 }
            "#]],
        );
    }

    #[test]
    fn init_struct_tuple_fileds_type_check() {
        check_in_root_file(
            r#"
                struct Point(int, int);
                fn main() {
                    Point(10, "aaa");
                }
            "#,
            expect![[r#"
                //- /main.nail
                struct Point(int, int);
                fn entry:main() -> () {
                    struct:Point(10, "aaa"); //: Point
                }

                ---
                error MismatchedTypeInitStructTuple: expected_ty: int, found_ty: string, init_struct: Point, found_arg_expr: `"aaa"`, arg_pos: 1
                ---
            "#]],
        );
    }

    #[test]
    fn init_struct_record_fileds() {
        check_in_root_file(
            r#"
                struct Point { x: int, y: string }
                fn main() {
                    Point { x: 10, y: "aaa" };
                    Point { y: "aaa", x: 10 };
                }
            "#,
            expect![[r#"
                //- /main.nail
                struct Point{ x: int, y: string }
                fn entry:main() -> () {
                    struct:Point { x: 10, y: "aaa" }; //: Point
                    struct:Point { y: "aaa", x: 10 }; //: Point
                }

                ---
                ---
            "#]],
        );
    }

    #[test]
    fn init_struct_record_fileds_missing_field_and_no_such_field() {
        check_in_root_file(
            r#"
                struct Point { x: int, y: int }
                fn main() {
                    Point { x: 10 };
                    Point { x: 10, y: 20, z: 30 };
                }
            "#,
            expect![[r#"
                //- /main.nail
                struct Point{ x: int, y: int }
                fn entry:main() -> () {
                    struct:Point { x: 10 }; //: Point
                    struct:Point { x: 10, y: 20, z: 30 }; //: Point
                }

                ---
                error MissingStructRecordField: missing_fields: ["y"], found_struct: Point
                error NoSuchStructRecordField: no_such_fields: ["z"], found_struct: Point
                ---
            "#]],
        );
    }

    #[test]
    fn init_struct_record_fileds_other_init() {
        check_in_root_file(
            r#"
                struct Point { x: int, y: int }
                fn main() {
                    Point;
                    Point(10, 20);
                }
            "#,
            expect![[r#"
                //- /main.nail
                struct Point{ x: int, y: int }
                fn entry:main() -> () {
                    struct:Point; //: <unknown>
                    struct:Point(10, 20); //: <unknown>
                }

                ---
                error NeededInitTupleOrRecord: found_ty: Point, found_expr: `struct:Point`, found_struct: Point
                error NotCallable: found_callee_ty: Point, found_callee_symbol: struct:Point
                ---
                error Type is unknown: expr: struct:Point
                error Type is unknown: expr: struct:Point(10, 20)
            "#]],
        );
    }

    #[test]
    fn init_struct_record_fileds_type_check() {
        check_in_root_file(
            r#"
                struct Point { x: int, y: string }
                fn main() {
                    Point { x: 10, y: "aaa" };
                    Point { x: "aaa", y: 10 };
                }
            "#,
            expect![[r#"
                //- /main.nail
                struct Point{ x: int, y: string }
                fn entry:main() -> () {
                    struct:Point { x: 10, y: "aaa" }; //: Point
                    struct:Point { x: "aaa", y: 10 }; //: Point
                }

                ---
                error MismatchedTypeInitStructRecord: expected_ty: int, found_ty: string, init_struct: Point, found_name: x, found_expr: `"aaa"`
                error MismatchedTypeInitStructRecord: expected_ty: string, found_ty: int, init_struct: Point, found_name: y, found_expr: `10`
                ---
            "#]],
        );
    }
}
