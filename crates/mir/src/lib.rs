use std::collections::HashMap;

use hir_ty::ResolvedType;
use la_arena::{Arena, Idx};

pub fn lower(hir_result: hir::LowerResult, hir_ty_result: hir_ty::TyLowerResult) -> LowerResult {
    let mir_lower = MirLower {
        hir_result,
        hir_ty_result,
    };

    mir_lower.lower()
}

struct MirLower {
    hir_result: hir::LowerResult,
    hir_ty_result: hir_ty::TyLowerResult,
}

impl MirLower {
    fn lower(self) -> LowerResult {
        let mut entry_point_idx = None;
        let mut bodies = vec![];
        for (idx, function) in self.hir_result.db.functions.iter() {
            let name = self
                .hir_result
                .interner
                .lookup(function.name.unwrap().key());
            if name == "main" {
                assert!(matches!(entry_point_idx, Some(_)));
                entry_point_idx = Some(bodies.len());
            }

            let body = self.lower_function_idx(idx);
            bodies.push(body);
        }

        LowerResult {
            entry_point_idx,
            bodies,
        }
    }

    fn lower_function_idx(&self, function_idx: hir::FunctionIdx) -> Body {
        let function = &self.hir_result.db.functions[function_idx];

        let body_block = self
            .hir_result
            .function_body_by_function(&function_idx)
            .unwrap();
        let body_block = match body_block {
            hir::Expr::Block(block) => block,
            _ => unreachable!(),
        };

        for stmt in &body_block.stmts {
            match stmt {
                hir::Stmt::VariableDef { name, value } => todo!(),
                hir::Stmt::Expr(_) => todo!(),
                hir::Stmt::FunctionDef { .. } => unreachable!(),
            }
        }

        Body {
            name: function.name.unwrap(),
            params: todo!(),
            variables: todo!(),
            blocks: todo!(),
            param_by_hir: todo!(),
            variable_by_hir: todo!(),
        }
    }
}

#[derive(Debug)]
pub struct LowerResult {
    entry_point_idx: Option<usize>,
    bodies: Vec<Body>,
}

impl LowerResult {
    fn entry_point(&self) -> Option<&Body> {
        self.entry_point_idx.map(|idx| &self.bodies[idx])
    }
}

#[derive(Debug)]
pub struct Body {
    name: hir::Name,
    params: Arena<Param>,
    param_by_hir: HashMap<Idx<hir::Param>, Idx<Param>>,
    variables: Arena<Variable>,
    variable_by_hir: HashMap<Idx<hir::Expr>, Idx<Variable>>,
    blocks: Arena<BasicBlock>,
}

#[derive(Debug)]
struct Param {
    ty: ResolvedType,
}

#[derive(Debug)]
struct Variable {}

#[derive(Debug)]
struct BasicBlock {
    kind: BasicBlockKind,
    statements: Vec<Idx<Statement>>,
    termination: Termination,
}

#[derive(Debug)]
enum Statement {}

#[derive(Debug)]
enum Termination {
    Return(Idx<Variable>),
    Goto(Idx<BasicBlock>),
}

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

    #[test]
    fn test_return() {
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
