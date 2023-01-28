use std::collections::HashMap;

use hir_ty::ResolvedType;
use la_arena::{Arena, Idx};

pub fn lower(hir_result: &hir::LowerResult, hir_ty_result: &hir_ty::TyLowerResult) -> LowerResult {
    let mir_lower = MirLower {
        hir_result,
        hir_ty_result,
    };

    mir_lower.lower()
}

struct MirLower<'a> {
    hir_result: &'a hir::LowerResult,
    hir_ty_result: &'a hir_ty::TyLowerResult,
}

impl<'a> MirLower<'a> {
    fn lower(self) -> LowerResult {
        let mut entry_point_idx = None;
        let mut bodies = vec![];
        for (idx, function) in self.hir_result.db.functions.iter() {
            let name = self
                .hir_result
                .interner
                .lookup(function.name.unwrap().key());
            if name == "main" {
                assert_eq!(entry_point_idx, None);
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
        let signature = &self.hir_ty_result.signature_by_function(&function_idx);

        let mut return_variable = Arena::<ReturnLocal>::new();
        let return_var_idx = return_variable.alloc(ReturnLocal {
            ty: signature.return_type,
            idx: 0,
        });

        let mut params = Arena::<Param>::new();
        let mut param_by_hir = HashMap::<hir::ParamIdx, Idx<Param>>::new();
        let mut idx = 1;
        for param in &function.params {
            let param_ty = self.hir_ty_result.type_by_param(*param);
            let param_idx = params.alloc(Param { ty: param_ty, idx });
            param_by_hir.insert(*param, param_idx);

            idx += 1;
        }

        let body_block = self
            .hir_result
            .function_body_by_function(&function_idx)
            .unwrap();
        let body_block = match body_block {
            hir::Expr::Block(block) => block,
            _ => unreachable!(),
        };

        let mut variables = Arena::new();
        let mut blocks = Arena::new();

        let entry_bb = BasicBlock {
            kind: BasicBlockKind::Entry,
            statements: vec![],
            termination: None,
            idx: 0,
        };
        let entry_bb_idx = blocks.alloc(entry_bb);

        let exit_bb = BasicBlock {
            kind: BasicBlockKind::Exit,
            statements: vec![],
            termination: Some(Termination::Return(return_var_idx)),
            idx: 1,
        };
        let exit_bb_idx = blocks.alloc(exit_bb);

        let is_returned = false;
        for stmt in &body_block.stmts {
            match stmt {
                hir::Stmt::VariableDef { name, value } => {
                    todo!()
                }
                hir::Stmt::Expr(expr) => {
                    todo!()
                }
                hir::Stmt::FunctionDef { .. } => unreachable!(),
            }
        }

        if !is_returned {
            let entry_bb = &mut blocks[entry_bb_idx];
            entry_bb.termination = Some(Termination::Goto(exit_bb_idx));
        }

        if let Some(tail) = body_block.tail {
            let tail = &self.hir_result.shared_ctx.exprs[tail];
            match tail {
                hir::Expr::Literal(literal) => {
                    let entry_bb = &mut blocks[entry_bb_idx];
                    match literal {
                        hir::Literal::Integer(value) => {
                            entry_bb.add_statement(Statement::Assign {
                                place: Place::ReturnLocal(return_var_idx),
                                value: Value::Constant(Constant::Integer(*value)),
                            });
                        }
                        hir::Literal::Bool(value) => {
                            entry_bb.add_statement(Statement::Assign {
                                place: Place::ReturnLocal(return_var_idx),
                                value: Value::Constant(Constant::Boolean(*value)),
                            });
                        }
                        _ => todo!(),
                    };
                }
                _ => todo!(),
            }
        }

        Body {
            name: function.name.unwrap(),
            params,
            return_variable,
            variables,
            blocks,
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
    return_variable: Arena<ReturnLocal>,
    variables: Arena<Local>,
    blocks: Arena<BasicBlock>,
}

#[derive(Debug)]
struct Param {
    ty: ResolvedType,
    idx: u64,
}

#[derive(Debug)]
struct Local {
    ty: ResolvedType,
    idx: u64,
}

#[derive(Debug)]
struct ReturnLocal {
    ty: ResolvedType,
    idx: u64,
}

#[derive(Debug)]
struct BasicBlock {
    kind: BasicBlockKind,
    statements: Vec<Statement>,
    termination: Option<Termination>,
    idx: u64,
}

impl BasicBlock {
    fn add_statement(&mut self, stmt: Statement) {
        self.statements.push(stmt);
    }
}

#[derive(Debug)]
enum Place {
    Param(Idx<Param>),
    Local(Idx<Local>),
    ReturnLocal(Idx<ReturnLocal>),
}

#[derive(Debug)]
enum Value {
    Constant(Constant),
}

#[derive(Debug)]
enum Constant {
    Integer(u64),
    Boolean(bool),
}

#[derive(Debug)]
enum Statement {
    Assign { place: Place, value: Value },
}

#[derive(Debug)]
enum Termination {
    Return(Idx<ReturnLocal>),
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
    use ast::AstNode;
    use expect_test::{expect, Expect};
    use hir_ty::ResolvedType;

    fn check(actual: &str, expect: Expect) {
        let parsed = parser::parse(actual);
        let ast = ast::SourceFile::cast(parsed.syntax()).unwrap();
        let hir_result = hir::lower(ast);
        let ty_hir_result = hir_ty::lower(&hir_result);

        let mir_result = crate::lower(&hir_result, &ty_hir_result);

        expect.assert_eq(&debug(&hir_result, &ty_hir_result, &mir_result));
    }

    fn indent(nesting: usize) -> String {
        "    ".repeat(nesting)
    }

    fn debug(
        hir_result: &hir::LowerResult,
        hir_ty_result: &hir_ty::TyLowerResult,
        mir_result: &crate::LowerResult,
    ) -> String {
        let mut msg = "".to_string();

        for body in &mir_result.bodies {
            let name = hir_result.interner.lookup(body.name.key());
            msg.push_str(&format!("fn {name}("));

            msg.push_str(&debug_params(
                body.params
                    .iter()
                    .map(|(_idx, param)| param)
                    .collect::<Vec<_>>(),
            ));

            let return_local = body.return_variable.iter().next().unwrap().1;
            msg.push_str(&format!(") -> {} {{\n", debug_ty(&return_local.ty)));

            msg.push_str(&format!(
                "{}let _{}: {}\n",
                indent(1),
                return_local.idx,
                debug_ty(&return_local.ty)
            ));

            for (_basic_block_idx, basic_block) in body.blocks.iter() {
                msg.push('\n');

                msg.push_str(&format!(
                    "{}{}: {{\n",
                    indent(1),
                    debug_bb_name(basic_block)
                ));

                for statement in &basic_block.statements {
                    msg.push_str(&format!(
                        "{}{}\n",
                        indent(2),
                        debug_statement(statement, body)
                    ));
                }

                if let Some(termination) = &basic_block.termination {
                    msg.push_str(&format!(
                        "{}{}\n",
                        indent(2),
                        debug_termination(termination, body)
                    ));
                }

                msg.push_str(&format!("{}}}\n", indent(1)));
            }

            msg.push_str("}\n");
        }

        msg
    }

    fn debug_params(params: Vec<&crate::Param>) -> String {
        params
            .iter()
            .map(|param| debug_param(param))
            .collect::<Vec<String>>()
            .join(", ")
    }

    fn debug_param(param: &crate::Param) -> String {
        format!("_{}: {}", param.idx, debug_ty(&param.ty))
    }

    fn debug_statement(statement: &crate::Statement, body: &crate::Body) -> String {
        match statement {
            crate::Statement::Assign { place, value } => {
                let place_msg = debug_place(place, body);
                let value_msg = debug_value(value);

                format!("{place_msg} = {value_msg}")
            }
        }
    }

    fn debug_place(place: &crate::Place, body: &crate::Body) -> String {
        match place {
            crate::Place::Param(param_idx) => {
                let param = &body.params[*param_idx];
                format!("_{}", param.idx)
            }
            crate::Place::Local(local_idx) => {
                let local = &body.variables[*local_idx];
                format!("_{}", local.idx)
            }
            crate::Place::ReturnLocal(return_local_idx) => {
                let return_local = &body.return_variable[*return_local_idx];
                format!("_{}", return_local.idx)
            }
        }
    }

    fn debug_value(value: &crate::Value) -> String {
        match value {
            crate::Value::Constant(constant) => match constant {
                crate::Constant::Integer(integer) => integer.to_string(),
                crate::Constant::Boolean(boolean) => boolean.to_string(),
            },
        }
    }

    fn debug_termination(termination: &crate::Termination, body: &crate::Body) -> String {
        match termination {
            crate::Termination::Return(return_local_idx) => {
                let return_local = &body.return_variable[*return_local_idx];
                format!("return _{}", return_local.idx)
            }
            crate::Termination::Goto(to_bb_idx) => {
                let to_bb = &body.blocks[*to_bb_idx];
                format!("goto -> {}", debug_bb_name(to_bb))
            }
        }
    }

    fn debug_ty(ty: &ResolvedType) -> String {
        match ty {
            ResolvedType::Unknown => "unknown",
            ResolvedType::Integer => "int",
            ResolvedType::String => "string",
            ResolvedType::Char => "char",
            ResolvedType::Bool => "bool",
            ResolvedType::Unit => "()",
            ResolvedType::Never => "!",
            ResolvedType::Function(_) => todo!(),
        }
        .to_string()
    }

    fn debug_bb_name(basic_block: &crate::BasicBlock) -> String {
        match basic_block.kind {
            crate::BasicBlockKind::Entry => "entry".to_string(),
            crate::BasicBlockKind::Exit => "exit".to_string(),
            crate::BasicBlockKind::Standard => {
                format!("bb{}", basic_block.idx)
            }
            crate::BasicBlockKind::Then => {
                format!("then{}", basic_block.idx)
            }
            crate::BasicBlockKind::Else => {
                format!("else{}", basic_block.idx)
            }
        }
    }

    #[test]
    fn test_main_return_int() {
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

    #[test]
    fn test_main_return_bool() {
        check(
            r#"
                fn main() -> bool {
                    true
                }
            "#,
            expect![[r#"
                fn main() -> bool {
                    let _0: bool

                    entry: {
                        _0 = true
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
