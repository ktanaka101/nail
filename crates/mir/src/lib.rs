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

    return_variable: Arena<ReturnLocal>,
    params: Arena<Param>,
    param_by_hir: HashMap<hir::ParamIdx, Idx<Param>>,
    variables: Arena<Local>,
    local_idx: u64,
    blocks: Arena<BasicBlock>,
    switch_idx: u64,
    current_bb: Option<Idx<BasicBlock>>,
    block_idx: u64,
    local_by_hir: HashMap<hir::ExprIdx, Idx<Local>>,
    exit_bb_idx: Option<Idx<BasicBlock>>,
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
            return_variable: Arena::new(),
            params: Arena::new(),
            param_by_hir: HashMap::new(),
            variables: Arena::new(),
            blocks: Arena::new(),
            switch_idx: 0,
            current_bb: None,
            block_idx: 0,
            local_by_hir: HashMap::new(),
            exit_bb_idx: None,
        }
    }

    fn return_variable_idx(&self) -> Idx<ReturnLocal> {
        self.return_variable
            .iter()
            .map(|(idx, _)| idx)
            .next()
            .unwrap()
    }

    fn exit_bb_idx(&self) -> Idx<BasicBlock> {
        self.exit_bb_idx.unwrap()
    }

    fn add_statement_to_current_bb(&mut self, statement: Statement) {
        let current_bb = &mut self.blocks[self.current_bb.unwrap()];
        current_bb.add_statement(statement);
    }

    fn add_termination_to_current_bb(&mut self, termination: Termination) {
        let current_bb = &mut self.blocks[self.current_bb.unwrap()];
        assert!(matches!(current_bb.termination, None));
        current_bb.termination = Some(termination);
    }

    fn alloc_local(&mut self, expr: Idx<hir::Expr>) -> Idx<Local> {
        let ty = self.hir_ty_result.type_by_expr(expr);
        let cond_local = Local {
            ty,
            idx: self.local_idx,
        };
        self.local_idx += 1;
        let local_idx = self.variables.alloc(cond_local);
        self.local_by_hir.insert(expr, local_idx);

        local_idx
    }

    fn get_local_by_expr(&self, expr: Idx<hir::Expr>) -> Idx<Local> {
        *self.local_by_hir.get(&expr).unwrap()
    }

    fn alloc_entry_bb(&mut self) -> Idx<BasicBlock> {
        let entry_bb = BasicBlock::new_entry_bb(0);
        self.blocks.alloc(entry_bb)
    }

    fn alloc_exit_bb(&mut self) -> Idx<BasicBlock> {
        assert!(matches!(self.exit_bb_idx, None));

        let mut exit_bb = BasicBlock::new_exit_bb(0);
        exit_bb.termination = Some(Termination::Return(self.return_variable_idx()));

        let exit_bb_idx = self.blocks.alloc(exit_bb);
        self.exit_bb_idx = Some(exit_bb_idx);

        exit_bb_idx
    }

    fn alloc_switch_bb(&mut self) -> AllocatedSwitchBB {
        let then_bb = BasicBlock::new_then_bb(self.switch_idx);
        let then_bb_idx = self.blocks.alloc(then_bb);

        let else_bb = BasicBlock::new_else_bb(self.switch_idx);
        let else_bb_idx = self.blocks.alloc(else_bb);

        self.switch_idx += 1;

        AllocatedSwitchBB {
            then_bb_idx,
            else_bb_idx,
        }
    }

    fn alloc_dest_bb_and_result_local(
        &mut self,
        expr: Idx<hir::Expr>,
    ) -> (Idx<BasicBlock>, Idx<Local>) {
        let dest_bb_idx = {
            let dest_bb = BasicBlock::new_standard_bb(self.block_idx);
            self.block_idx += 1;
            self.blocks.alloc(dest_bb)
        };

        let result_local_idx = self.alloc_local(expr);

        (dest_bb_idx, result_local_idx)
    }

    fn alloc_standard_bb(&mut self) -> Idx<BasicBlock> {
        let dest_bb = BasicBlock::new_standard_bb(self.block_idx);
        self.block_idx += 1;
        self.blocks.alloc(dest_bb)
    }

    fn lower_expr(&mut self, expr: hir::ExprIdx) -> LoweredExpr {
        let expr = &self.hir_result.shared_ctx.exprs[expr];
        match expr {
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Integer(value) => {
                    LoweredExpr::Value(Value::Constant(Constant::Integer(*value)))
                }
                hir::Literal::Bool(value) => {
                    LoweredExpr::Value(Value::Constant(Constant::Boolean(*value)))
                }
                _ => todo!(),
            },
            hir::Expr::VariableRef { var } => match var {
                hir::Symbol::Param { name, param } => todo!(),
                hir::Symbol::Local { name, expr } => {
                    LoweredExpr::Value(Value::Place(Place::Local(self.get_local_by_expr(*expr))))
                }
                hir::Symbol::Function { name, function } => todo!(),
                hir::Symbol::Missing { name } => todo!(),
            },
            hir::Expr::Return { value } => {
                if let Some(value) = value {
                    let value = match self.lower_expr(*value) {
                        LoweredExpr::Value(value) => value,
                        LoweredExpr::Return => return LoweredExpr::Return,
                    };
                    let return_value_place = Place::ReturnLocal(self.return_variable_idx());
                    self.add_statement_to_current_bb(Statement::Assign {
                        place: return_value_place,
                        value,
                    });
                }

                LoweredExpr::Return
            }
            hir::Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_local_idx = {
                    let cond_local_idx = self.alloc_local(*condition);
                    let cond_value = match self.lower_expr(*condition) {
                        LoweredExpr::Value(value) => value,
                        LoweredExpr::Return => return LoweredExpr::Return,
                    };
                    let place = Place::Local(cond_local_idx);
                    self.add_statement_to_current_bb(Statement::Assign {
                        place,
                        value: cond_value,
                    });

                    cond_local_idx
                };

                let mut dest_bb_and_result_local_idx: Option<(Idx<BasicBlock>, Idx<Local>)> = None;

                let switch_bb = self.alloc_switch_bb();
                self.add_termination_to_current_bb(Termination::Switch {
                    condition: Place::Local(cond_local_idx),
                    then_bb: switch_bb.then_bb_idx,
                    else_bb: switch_bb.else_bb_idx,
                });

                {
                    self.current_bb = Some(switch_bb.then_bb_idx);

                    let then_block = match &self.hir_result.shared_ctx.exprs[*then_branch] {
                        hir::Expr::Block(block) => block,
                        _ => unreachable!(),
                    };

                    let mut has_return = false;
                    for stmt in &then_block.stmts {
                        match self.lower_stmt(stmt) {
                            LoweredStmt::Return => {
                                has_return = true;
                                break;
                            }
                            LoweredStmt::Unit => (),
                        }
                    }
                    if !has_return {
                        if let Some(tail) = then_block.tail {
                            match self.lower_expr(tail) {
                                LoweredExpr::Value(value) => {
                                    let (dest_bb_idx, result_local_idx) =
                                        match dest_bb_and_result_local_idx {
                                            Some(idxes) => idxes,
                                            None => {
                                                let idxes = self
                                                    .alloc_dest_bb_and_result_local(*then_branch);
                                                dest_bb_and_result_local_idx = Some(idxes);

                                                idxes
                                            }
                                        };

                                    self.add_statement_to_current_bb(Statement::Assign {
                                        place: Place::Local(result_local_idx),
                                        value,
                                    });
                                    self.add_termination_to_current_bb(Termination::Goto(
                                        dest_bb_idx,
                                    ));
                                }
                                LoweredExpr::Return => {
                                    self.add_termination_to_current_bb(Termination::Goto(
                                        self.exit_bb_idx(),
                                    ));
                                }
                            };
                        }
                    }
                }

                {
                    self.current_bb = Some(switch_bb.else_bb_idx);

                    match else_branch {
                        Some(else_block) => {
                            let else_block = match &self.hir_result.shared_ctx.exprs[*else_block] {
                                hir::Expr::Block(block) => block,
                                _ => unreachable!(),
                            };

                            let mut has_return = false;
                            for stmt in &else_block.stmts {
                                match self.lower_stmt(stmt) {
                                    LoweredStmt::Return => {
                                        has_return = true;
                                        break;
                                    }
                                    LoweredStmt::Unit => (),
                                }
                            }

                            if !has_return {
                                if let Some(tail) = else_block.tail {
                                    match self.lower_expr(tail) {
                                        LoweredExpr::Value(value) => {
                                            let (dest_bb_idx, result_local_idx) =
                                                match dest_bb_and_result_local_idx {
                                                    Some(idxes) => idxes,
                                                    None => {
                                                        let idxes = self
                                                            .alloc_dest_bb_and_result_local(
                                                                *then_branch,
                                                            );
                                                        dest_bb_and_result_local_idx = Some(idxes);

                                                        idxes
                                                    }
                                                };

                                            self.add_statement_to_current_bb(Statement::Assign {
                                                place: Place::Local(result_local_idx),
                                                value,
                                            });
                                            self.add_termination_to_current_bb(Termination::Goto(
                                                dest_bb_idx,
                                            ));
                                        }
                                        LoweredExpr::Return => {
                                            self.add_termination_to_current_bb(Termination::Goto(
                                                self.exit_bb_idx(),
                                            ));
                                        }
                                    };
                                }
                            }

                            if let Some((dest_bb_idx, result_local_idx)) =
                                dest_bb_and_result_local_idx
                            {
                                self.current_bb = Some(dest_bb_idx);
                                LoweredExpr::Value(Value::Place(Place::Local(result_local_idx)))
                            } else {
                                LoweredExpr::Return
                            }
                        }
                        None => match dest_bb_and_result_local_idx {
                            Some((dest_bb_idx, result_local_idx)) => {
                                let unit = Value::Constant(Constant::Unit);
                                self.add_statement_to_current_bb(Statement::Assign {
                                    place: Place::Local(result_local_idx),
                                    value: unit,
                                });

                                self.add_termination_to_current_bb(Termination::Goto(dest_bb_idx));
                                self.current_bb = Some(dest_bb_idx);

                                LoweredExpr::Value(Value::Place(Place::Local(result_local_idx)))
                            }
                            None => {
                                let dest_bb_idx = self.alloc_standard_bb();
                                self.add_termination_to_current_bb(Termination::Goto(dest_bb_idx));
                                self.current_bb = Some(dest_bb_idx);

                                LoweredExpr::Value(Value::Constant(Constant::Unit))
                            }
                        },
                    }
                }
            }
            _ => todo!(),
        }
    }

    fn lower_stmt(&mut self, stmt: &hir::Stmt) -> LoweredStmt {
        match stmt {
            hir::Stmt::VariableDef { name: _, value } => {
                let local_idx = self.alloc_local(*value);
                let value = match self.lower_expr(*value) {
                    LoweredExpr::Value(value) => value,
                    LoweredExpr::Return => {
                        return LoweredStmt::Return;
                    }
                };
                self.add_statement_to_current_bb(Statement::Assign {
                    place: Place::Local(local_idx),
                    value,
                });
            }
            hir::Stmt::Expr(expr) => {
                match self.lower_expr(*expr) {
                    LoweredExpr::Value(value) => value,
                    LoweredExpr::Return => {
                        return LoweredStmt::Return;
                    }
                };
            }
            hir::Stmt::FunctionDef { .. } => unreachable!(),
        }

        LoweredStmt::Unit
    }

    fn lower(mut self) -> Body {
        let function = &self.hir_result.db.functions[self.function_idx];
        let signature = &self.hir_ty_result.signature_by_function(&self.function_idx);

        self.return_variable.alloc(ReturnLocal {
            ty: signature.return_type,
            idx: 0,
        });

        for param in &function.params {
            let param_ty = self.hir_ty_result.type_by_param(*param);
            let param_idx = self.params.alloc(Param {
                ty: param_ty,
                idx: self.local_idx,
            });
            self.param_by_hir.insert(*param, param_idx);

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

        let entry_bb_idx = self.alloc_entry_bb();
        let exit_bb_idx = self.alloc_exit_bb();

        self.current_bb = Some(entry_bb_idx);

        let mut has_return = false;
        for stmt in &body_block.stmts {
            match self.lower_stmt(stmt) {
                LoweredStmt::Return => {
                    has_return = true;
                    break;
                }
                LoweredStmt::Unit => (),
            }
        }

        if !has_return {
            if let Some(tail) = body_block.tail {
                match self.lower_expr(tail) {
                    LoweredExpr::Value(value) => {
                        self.add_statement_to_current_bb(Statement::Assign {
                            place: Place::ReturnLocal(self.return_variable_idx()),
                            value,
                        });
                        self.add_termination_to_current_bb(Termination::Goto(exit_bb_idx));
                        self.current_bb = Some(exit_bb_idx);
                    }
                    LoweredExpr::Return => (),
                };
            }
        } else {
            self.add_termination_to_current_bb(Termination::Goto(exit_bb_idx));
            self.current_bb = Some(exit_bb_idx);
        }

        Body {
            name: function.name.unwrap(),
            params: self.params,
            return_variable: self.return_variable,
            variables: self.variables,
            blocks: self.blocks,
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
    fn new_entry_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Entry,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_exit_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Exit,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_then_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Then,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_else_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Else,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn new_standard_bb(idx: u64) -> Self {
        Self {
            kind: BasicBlockKind::Standard,
            statements: vec![],
            termination: None,
            idx,
        }
    }

    fn add_statement(&mut self, stmt: Statement) {
        self.statements.push(stmt);
    }
}

struct AllocatedSwitchBB {
    then_bb_idx: Idx<BasicBlock>,
    else_bb_idx: Idx<BasicBlock>,
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
    Place(Place),
}

#[derive(Debug)]
enum LoweredExpr {
    Return,
    Value(Value),
}

#[derive(Debug)]
enum LoweredStmt {
    Return,
    Unit,
}

#[derive(Debug)]
enum Constant {
    Integer(u64),
    Boolean(bool),
    Unit,
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
                let value_msg = debug_value(value, body);

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

    fn debug_value(value: &crate::Value, body: &crate::Body) -> String {
        match value {
            crate::Value::Constant(constant) => {
                let const_value = match constant {
                    crate::Constant::Integer(integer) => integer.to_string(),
                    crate::Constant::Boolean(boolean) => boolean.to_string(),
                    crate::Constant::Unit => "()".to_string(),
                };
                format!("const {const_value}")
            }
            crate::Value::Place(place) => debug_place(place, body),
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
                        _0 = const 10
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
                        _0 = const true
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
    fn test_let() {
        check(
            r#"
                fn main() -> int {
                    let x = 10
                    x
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = const 10
                        _0 = _1
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
    fn test_return() {
        check(
            r#"
                fn main() -> int {
                    return 10
                    20
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int

                    entry: {
                        _0 = const 10
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
    fn test_return_in_switch() {
        check(
            r#"
                fn main() -> int {
                    if true {
                        return 10
                    } else {
                        return 20
                    }
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: bool

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _0 = const 10
                        goto -> exit
                    }

                    else0: {
                        _0 = const 20
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn return_value_when_false_in_switch() {
        check(
            r#"
                fn main() -> int {
                    if true {
                        10
                    } else {
                        return 20
                    }
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _2 = const 10
                        goto -> bb0
                    }

                    else0: {
                        _0 = const 20
                        goto -> exit
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn return_value_when_true_in_switch() {
        check(
            r#"
                fn main() -> int {
                    if true {
                        return 10
                    } else {
                        20
                    }
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: !

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _0 = const 10
                        goto -> exit
                    }

                    else0: {
                        _2 = const 20
                        goto -> bb0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
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
                    let _2: int

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _2 = const 10
                        goto -> bb0
                    }

                    else0: {
                        _2 = const 20
                        goto -> bb0
                    }

                    bb0: {
                        _0 = _2
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_switch_conditional_early_return() {
        check(
            r#"
                fn main() -> int {
                    let a = 10
                    if true {
                        return 20
                    }

                    a
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int
                    let _2: bool

                    entry: {
                        _1 = const 10
                        _2 = const true
                        switch(_2) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _0 = const 20
                        goto -> exit
                    }

                    else0: {
                        goto -> bb0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_switch_only_then_branch() {
        check(
            r#"
                fn main() -> int {
                    let a = 10
                    if true {
                        20
                    }

                    a
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int
                    let _2: bool
                    let _3: int

                    entry: {
                        _1 = const 10
                        _2 = const true
                        switch(_2) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _3 = const 20
                        goto -> bb0
                    }

                    else0: {
                        _3 = const ()
                        goto -> bb0
                    }

                    bb0: {
                        _0 = _1
                        goto -> exit
                    }
                }
            "#]],
        );
    }

    #[test]
    fn test_statements_in_switch() {
        check(
            r#"
                fn main() -> int {
                    if true {
                        let b = 20
                        b
                    } else {
                        let c = 20
                        c
                    }
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: bool
                    let _2: int
                    let _3: int
                    let _4: int

                    entry: {
                        _1 = const true
                        switch(_1) -> [true: then0, false: else0]
                    }

                    exit: {
                        return _0
                    }

                    then0: {
                        _2 = const 20
                        _3 = _2
                        goto -> bb0
                    }

                    else0: {
                        _4 = const 20
                        _3 = _4
                        goto -> bb0
                    }

                    bb0: {
                        _0 = _3
                        goto -> exit
                    }
                }
            "#]],
        );
    }
}
