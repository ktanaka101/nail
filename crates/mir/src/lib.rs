use la_arena::Idx;

pub fn lower(hir_result: hir::LowerResult, hir_ty_result: hir_ty::TyLowerResult) -> LowerResult {
    todo!()
}

#[derive(Debug)]
pub struct LowerResult {
    bodies: Vec<Body>,
}

#[derive(Debug)]
struct Body {
    params: Vec<Idx<Param>>,
    variables: Vec<Idx<Variable>>,
    blocks: Vec<Idx<BasicBlock>>,
}

#[derive(Debug)]
struct Param {}

#[derive(Debug)]
struct Variable {}

#[derive(Debug)]
struct BasicBlock {
    kind: BasicBlockKind,
    statements: Vec<Idx<Stmt>>,
}

#[derive(Debug)]
enum Stmt {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum BasicBlockKind {
    Entry,
    Exit,
    Standard,
    Then,
    Else,
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    fn check(actual: &str, expected: Expect) {
        expected.assert_eq(actual);
    }

    fn test_switch() {
        check(
            r#"
                fn main() -> int {
                    10
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int

                    entry: {
                        _0 = 10
                        goto -> exit
                    }
                    exit: {
                        return _0
                    }
                }
            "#]],
        );
    }

    // #[test]
    // fn test_switch() {
    //     check(
    //         r#"
    //             fn main() -> int {
    //                 if true {
    //                     10
    //                 } else {
    //                     20
    //                 }
    //             }
    //         "#,
    //         expect![[r#"
    //             fn main() -> int {
    //                 let _0: int
    //                 let _1: bool

    //                 entry: {
    //                     switch(_1) -> [true: then0, else: else0]
    //                 }
    //                 then0: {
    //                     _0 = 10
    //                     goto -> exit
    //                 }
    //                 else0: {
    //                     _0 = 20
    //                     goto -> exit
    //                 }
    //                 exit: {
    //                     return _0
    //                 }
    //             }
    //         "#]],
    //     );
    // }
}
