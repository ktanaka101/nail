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

struct FunctionLower<'a> {
    hir_result: &'a hir::LowerResult,
    hir_ty_result: &'a hir_ty::TyLowerResult,
    function_idx: Idx<hir::Function>,
    local_idx: u64,
}

impl<'a> FunctionLower<'a> {
    fn new(
        hir_result: &'a hir::LowerResult,
        hir_ty_result: &'a hir_ty::TyLowerResult,
        function_idx: Idx<hir::Function>,
    ) -> Self {
        FunctionLower {
            hir_result,
            hir_ty_result,
            function_idx,
            local_idx: 1,
        }
    }

    fn lower(mut self) -> Body {
        let function = &self.hir_result.db.functions[self.function_idx];
        let signature = &self.hir_ty_result.signature_by_function(&self.function_idx);

        let mut return_variable = Arena::<ReturnLocal>::new();
        let return_var_idx = return_variable.alloc(ReturnLocal {
            ty: signature.return_type,
            idx: 0,
        });

        let mut params = Arena::<Param>::new();
        let mut param_by_hir = HashMap::<hir::ParamIdx, Idx<Param>>::new();
        for param in &function.params {
            let param_ty = self.hir_ty_result.type_by_param(*param);
            let param_idx = params.alloc(Param {
                ty: param_ty,
                idx: self.local_idx,
            });
            param_by_hir.insert(*param, param_idx);

            self.local_idx += 1;
        }

        let body_block = self
            .hir_result
            .function_body_by_function(&self.function_idx)
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

        let mut switch_idx = 0;

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
                hir::Expr::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let cond_ty = self.hir_ty_result.type_by_expr(*condition);
                    let cond_tmp_var = Local {
                        ty: cond_ty,
                        idx: self.local_idx,
                    };
                    self.local_idx += 1;

                    let cond_idx = variables.alloc(cond_tmp_var);

                    let cond = &self.hir_result.shared_ctx.exprs[*condition];
                    match cond {
                        hir::Expr::Literal(literal) => {
                            match literal {
                                hir::Literal::Bool(value) => {
                                    let entry_bb = &mut blocks[entry_bb_idx];
                                    entry_bb.add_statement(Statement::Assign {
                                        place: Place::Local(cond_idx),
                                        value: Value::Constant(Constant::Boolean(*value)),
                                    });
                                }
                                _ => todo!(),
                            };
                        }
                        _ => todo!(),
                    }

                    let then_block = match &self.hir_result.shared_ctx.exprs[*then_branch] {
                        hir::Expr::Block(block) => block,
                        _ => unreachable!(),
                    };
                    let else_block = match else_branch {
                        Some(else_block) => match &self.hir_result.shared_ctx.exprs[*else_block] {
                            hir::Expr::Block(block) => block,
                            _ => unreachable!(),
                        },
                        None => todo!(),
                    };

                    let mut then_bb = BasicBlock {
                        kind: BasicBlockKind::Then,
                        statements: vec![],
                        termination: Some(Termination::Goto(exit_bb_idx)),
                        idx: switch_idx,
                    };

                    let then_tail_expr = match then_block.tail {
                        Some(tail) => &self.hir_result.shared_ctx.exprs[tail],
                        None => unimplemented!(),
                    };
                    match then_tail_expr {
                        hir::Expr::Literal(literal) => match literal {
                            hir::Literal::Integer(value) => {
                                then_bb.add_statement(Statement::Assign {
                                    place: Place::ReturnLocal(return_var_idx),
                                    value: Value::Constant(Constant::Integer(*value)),
                                });
                            }
                            _ => todo!(),
                        },
                        _ => todo!(),
                    };
                    let then_bb_idx = blocks.alloc(then_bb);

                    let mut else_bb = BasicBlock {
                        kind: BasicBlockKind::Else,
                        statements: vec![],
                        termination: Some(Termination::Goto(exit_bb_idx)),
                        idx: switch_idx,
                    };

                    switch_idx += 1;

                    let then_tail_expr = match else_block.tail {
                        Some(tail) => &self.hir_result.shared_ctx.exprs[tail],
                        None => unimplemented!(),
                    };
                    match then_tail_expr {
                        hir::Expr::Literal(literal) => match literal {
                            hir::Literal::Integer(value) => {
                                else_bb.add_statement(Statement::Assign {
                                    place: Place::ReturnLocal(return_var_idx),
                                    value: Value::Constant(Constant::Integer(*value)),
                                });
                            }
                            _ => todo!(),
                        },
                        _ => todo!(),
                    };
                    let else_bb_idx = blocks.alloc(else_bb);

                    let entry_bb = &mut blocks[entry_bb_idx];
                    entry_bb.termination = Some(Termination::Switch {
                        condition: Place::Local(cond_idx),
                        then_bb: then_bb_idx,
                        else_bb: else_bb_idx,
                    });
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

struct MirLower<'a> {
    hir_result: &'a hir::LowerResult,
    hir_ty_result: &'a hir_ty::TyLowerResult,
}

impl<'a> MirLower<'a> {
    fn lower(self) -> LowerResult {
        let mut entry_point_idx = None;
        let mut bodies = vec![];
        for (function_idx, function) in self.hir_result.db.functions.iter() {
            let name = self
                .hir_result
                .interner
                .lookup(function.name.unwrap().key());
            if name == "main" {
                assert_eq!(entry_point_idx, None);
                entry_point_idx = Some(bodies.len());
            }

            let lower = FunctionLower::new(self.hir_result, self.hir_ty_result, function_idx);
            let body = lower.lower();
            bodies.push(body);
        }

        LowerResult {
            entry_point_idx,
            bodies,
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

#[derive(Debug, Clone, Copy)]
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
    Switch {
        condition: Place,
        then_bb: Idx<BasicBlock>,
        else_bb: Idx<BasicBlock>,
    },
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

            for (_variable_idx, variable) in body.variables.iter() {
                msg.push_str(&format!(
                    "{}let _{}: {}\n",
                    indent(1),
                    variable.idx,
                    debug_ty(&variable.ty)
                ));
            }

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
            crate::Termination::Switch {
                condition,
                then_bb,
                else_bb,
            } => {
                let condition = debug_place(condition, body);
                let then_bb_name = debug_bb_name_by_idx(*then_bb, body);
                let else_bb_name = debug_bb_name_by_idx(*else_bb, body);
                format!("switch({condition}) -> [true: {then_bb_name}, false: {else_bb_name}]")
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

    fn debug_bb_name_by_idx(
        basic_block_idx: crate::Idx<crate::BasicBlock>,
        body: &crate::Body,
    ) -> String {
        let basic_block = &body.blocks[basic_block_idx];
        debug_bb_name(basic_block)
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

    #[test]
    fn test_switch() {
        check(
            r#"
                fn main() -> int {
                    if true {
                        10
                    } else {
                        20
                    }
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: bool

                    entry: {
                        _1 = true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _0 = 10
                        goto -> exit
                    }

                    else0: {
                        _0 = 20
                        goto -> exit
                    }
                }
            "#]],
        );
    }
}
