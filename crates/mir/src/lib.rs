use std::{collections::HashMap, fmt::Binary};

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
    function_id_by_hir_function: &'a HashMap<Idx<hir::Function>, FunctionId>,
    function_idx: Idx<hir::Function>,

    return_local: Idx<Local>,
    params: Arena<Param>,
    param_by_hir: HashMap<hir::ParamIdx, Idx<Param>>,
    locals: Arena<Local>,
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
        function_id_by_hir_function: &'a HashMap<Idx<hir::Function>, FunctionId>,
        function_idx: Idx<hir::Function>,
    ) -> Self {
        let mut locals = Arena::new();
        let signature = &hir_ty_result.signature_by_function(&function_idx);
        let return_local = locals.alloc(Local {
            ty: signature.return_type,
            idx: 0,
        });

        FunctionLower {
            hir_result,
            hir_ty_result,
            function_id_by_hir_function,
            function_idx,
            local_idx: 1,
            return_local,
            params: Arena::new(),
            param_by_hir: HashMap::new(),
            locals,
            blocks: Arena::new(),
            switch_idx: 0,
            current_bb: None,
            block_idx: 0,
            local_by_hir: HashMap::new(),
            exit_bb_idx: None,
        }
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
        let local_idx = self.locals.alloc(cond_local);
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
        exit_bb.termination = Some(Termination::Return(self.return_local));

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

    fn lower_expr(&mut self, expr_idx: hir::ExprIdx) -> LoweredExpr {
        let expr = &self.hir_result.shared_ctx.exprs[expr_idx];
        match expr {
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Integer(value) => {
                    LoweredExpr::Operand(Operand::Constant(Constant::Integer(*value)))
                }
                hir::Literal::Bool(value) => {
                    LoweredExpr::Operand(Operand::Constant(Constant::Boolean(*value)))
                }
                _ => todo!(),
            },
            hir::Expr::VariableRef { var } => match var {
                hir::Symbol::Param { name, param } => todo!(),
                hir::Symbol::Local { name, expr } => LoweredExpr::Operand(Operand::Place(
                    Place::Local(self.get_local_by_expr(*expr)),
                )),
                hir::Symbol::Function { name, function } => todo!(),
                hir::Symbol::Missing { name } => todo!(),
            },
            hir::Expr::Return { value } => {
                if let Some(value) = value {
                    let operand = match self.lower_expr(*value) {
                        LoweredExpr::Operand(operand) => operand,
                        LoweredExpr::Return => return LoweredExpr::Return,
                    };
                    let return_value_place = Place::Local(self.return_local);
                    self.add_statement_to_current_bb(Statement::Assign {
                        place: return_value_place,
                        value: operand.into(),
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
                    let cond_operand = match self.lower_expr(*condition) {
                        LoweredExpr::Operand(operand) => operand,
                        LoweredExpr::Return => return LoweredExpr::Return,
                    };
                    let place = Place::Local(cond_local_idx);
                    self.add_statement_to_current_bb(Statement::Assign {
                        place,
                        value: cond_operand.into(),
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
                                LoweredExpr::Operand(operand) => {
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
                                        value: operand.into(),
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
                                        LoweredExpr::Operand(operand) => {
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
                                                value: operand.into(),
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
                                LoweredExpr::Operand(Operand::Place(Place::Local(result_local_idx)))
                            } else {
                                LoweredExpr::Return
                            }
                        }
                        None => match dest_bb_and_result_local_idx {
                            Some((dest_bb_idx, result_local_idx)) => {
                                let unit = Operand::Constant(Constant::Unit);
                                self.add_statement_to_current_bb(Statement::Assign {
                                    place: Place::Local(result_local_idx),
                                    value: unit.into(),
                                });

                                self.add_termination_to_current_bb(Termination::Goto(dest_bb_idx));
                                self.current_bb = Some(dest_bb_idx);

                                LoweredExpr::Operand(Operand::Place(Place::Local(result_local_idx)))
                            }
                            None => {
                                let dest_bb_idx = self.alloc_standard_bb();
                                self.add_termination_to_current_bb(Termination::Goto(dest_bb_idx));
                                self.current_bb = Some(dest_bb_idx);

                                LoweredExpr::Operand(Operand::Constant(Constant::Unit))
                            }
                        },
                    }
                }
            }
            hir::Expr::Binary { op, lhs, rhs } => {
                let lhs = match self.lower_expr(*lhs) {
                    LoweredExpr::Return => return LoweredExpr::Return,
                    LoweredExpr::Operand(operand) => operand,
                };

                let rhs = match self.lower_expr(*rhs) {
                    LoweredExpr::Return => return LoweredExpr::Return,
                    LoweredExpr::Operand(operand) => operand,
                };

                let op = match op {
                    hir::BinaryOp::Add => BinaryOp::Add,
                    hir::BinaryOp::Sub => BinaryOp::Sub,
                    hir::BinaryOp::Mul => BinaryOp::Mul,
                    hir::BinaryOp::Div => BinaryOp::Div,
                    hir::BinaryOp::Equal => todo!(),
                };

                let local = self.alloc_local(expr_idx);
                let value = Value::BinaryOp {
                    op,
                    left: lhs,
                    right: rhs,
                };
                let place = Place::Local(local);
                self.add_statement_to_current_bb(Statement::Assign { place, value });

                LoweredExpr::Operand(Operand::Place(place))
            }
            hir::Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.lower_stmt(stmt);
                }

                if let Some(tail) = block.tail {
                    self.lower_expr(tail)
                } else {
                    LoweredExpr::Operand(Operand::Constant(Constant::Unit))
                }
            }
            hir::Expr::Call { callee, args } => {
                let mut mir_args = vec![];
                for arg in args {
                    match self.lower_expr(*arg) {
                        LoweredExpr::Return => return LoweredExpr::Return,
                        LoweredExpr::Operand(operand) => {
                            mir_args.push(operand);
                        }
                    }
                }

                match callee {
                    hir::Symbol::Param { name, param } => unimplemented!(),
                    hir::Symbol::Local { name, expr } => unimplemented!(),
                    hir::Symbol::Function { name, function } => {
                        todo!()
                    }
                    hir::Symbol::Missing { name } => unreachable!(),
                }
            }
            _ => todo!(),
        }
    }

    fn lower_stmt(&mut self, stmt: &hir::Stmt) -> LoweredStmt {
        match stmt {
            hir::Stmt::VariableDef { name: _, value } => {
                let local_idx = self.alloc_local(*value);
                let operand = match self.lower_expr(*value) {
                    LoweredExpr::Operand(operand) => operand,
                    LoweredExpr::Return => {
                        return LoweredStmt::Return;
                    }
                };
                self.add_statement_to_current_bb(Statement::Assign {
                    place: Place::Local(local_idx),
                    value: operand.into(),
                });
            }
            hir::Stmt::ExprStmt { expr, .. } => {
                match self.lower_expr(*expr) {
                    LoweredExpr::Operand(operand) => operand,
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
                    LoweredExpr::Operand(operand) => {
                        self.add_statement_to_current_bb(Statement::Assign {
                            place: Place::Local(self.return_local),
                            value: operand.into(),
                        });
                        self.add_termination_to_current_bb(Termination::Goto(exit_bb_idx));
                        self.current_bb = Some(exit_bb_idx);
                    }
                    LoweredExpr::Return => (),
                };
            } else {
                self.add_termination_to_current_bb(Termination::Goto(exit_bb_idx));
                self.current_bb = Some(exit_bb_idx);
            }
        } else {
            self.add_termination_to_current_bb(Termination::Goto(exit_bb_idx));
            self.current_bb = Some(exit_bb_idx);
        }

        Body {
            name: function.name.unwrap(),
            params: self.params,
            return_local: self.return_local,
            locals: self.locals,
            blocks: self.blocks,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct FunctionId(u32);

struct FunctionIdGenerator {
    id: u32,
}
impl FunctionIdGenerator {
    fn new() -> Self {
        Self { id: 0 }
    }

    fn gen(&mut self) -> FunctionId {
        let id = self.id;
        self.id += 1;

        FunctionId(id)
    }
}

struct MirLower<'a> {
    hir_result: &'a hir::LowerResult,
    hir_ty_result: &'a hir_ty::TyLowerResult,
}

impl<'a> MirLower<'a> {
    fn lower(self) -> LowerResult {
        let mut entry_point: Option<Idx<Body>> = None;
        let mut bodies = Arena::new();

        let function_id_by_hir_function = {
            let mut function_id_resolver = FunctionIdGenerator::new();
            let mut function_id_by_hir_function = HashMap::<Idx<hir::Function>, FunctionId>::new();
            for (function_idx, _function) in self.hir_result.db.functions.iter() {
                function_id_by_hir_function.insert(function_idx, function_id_resolver.gen());
            }

            function_id_by_hir_function
        };

        let mut body_by_function = HashMap::<FunctionId, Idx<Body>>::new();
        for (function_idx, function) in self.hir_result.db.functions.iter() {
            let lower = FunctionLower::new(
                self.hir_result,
                self.hir_ty_result,
                &function_id_by_hir_function,
                function_idx,
            );
            let body = lower.lower();
            let body_idx = bodies.alloc(body);

            body_by_function.insert(
                *function_id_by_hir_function.get(&function_idx).unwrap(),
                body_idx,
            );

            let name = self
                .hir_result
                .interner
                .lookup(function.name.unwrap().key());
            if name == "main" {
                assert_eq!(entry_point, None);
                entry_point = Some(body_idx);
            }
        }

        LowerResult {
            entry_point,
            bodies,
            body_by_function,
        }
    }
}

#[derive(Debug)]
pub struct LowerResult {
    entry_point: Option<Idx<Body>>,
    bodies: Arena<Body>,
    body_by_function: HashMap<FunctionId, Idx<Body>>,
}

impl LowerResult {
    fn entry_point(&self) -> Option<&Body> {
        self.entry_point.map(|idx| &self.bodies[idx])
    }

    fn body_by_function(&self, function_id: FunctionId) -> &Body {
        let body_idx = self.body_by_function.get(&function_id).unwrap();
        &self.bodies[*body_idx]
    }
}

#[derive(Debug)]
pub struct Body {
    name: hir::Name,
    params: Arena<Param>,
    return_local: Idx<Local>,
    locals: Arena<Local>,
    blocks: Arena<BasicBlock>,
}
impl Body {
    fn signature(&self) -> Signature {
        let params = self.params.iter().map(|(idx, _param)| idx).collect();
        Signature {
            params,
            return_value: self.return_local,
        }
    }
}

#[derive(Debug)]
struct Signature {
    params: Vec<Idx<Param>>,
    return_value: Idx<Local>,
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
}

#[derive(Debug)]
enum Value {
    Operand(Operand),
    BinaryOp {
        op: BinaryOp,
        left: Operand,
        right: Operand,
    },
}

#[derive(Debug)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
enum Operand {
    Place(Place),
    Constant(Constant),
}
impl From<Operand> for Value {
    fn from(value: Operand) -> Self {
        Self::Operand(value)
    }
}

#[derive(Debug)]
enum LoweredExpr {
    Return,
    Operand(Operand),
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
    Return(Idx<Local>),
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

        for (_body_idx, body) in mir_result.bodies.iter() {
            let name = hir_result.interner.lookup(body.name.key());
            msg.push_str(&format!("fn {name}("));

            msg.push_str(&debug_params(
                body.params
                    .iter()
                    .map(|(_idx, param)| param)
                    .collect::<Vec<_>>(),
            ));

            let return_local = &body.locals[body.return_local];
            msg.push_str(&format!(") -> {} {{\n", debug_ty(&return_local.ty)));

            for (_variable_idx, variable) in body.locals.iter() {
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
                let local = &body.locals[*local_idx];
                format!("_{}", local.idx)
            }
        }
    }

    fn debug_value(value: &crate::Value, body: &crate::Body) -> String {
        match value {
            crate::Value::Operand(operand) => debug_operand(operand, body),
            crate::Value::BinaryOp { op, left, right } => {
                let function_name = match op {
                    crate::BinaryOp::Add => "add",
                    crate::BinaryOp::Sub => "sub",
                    crate::BinaryOp::Mul => "mul",
                    crate::BinaryOp::Div => "div",
                }
                .to_string();
                let left = debug_operand(left, body);
                let right = debug_operand(right, body);

                format!("{function_name}({left}, {right})")
            }
        }
    }

    fn debug_constant(constant: &crate::Constant) -> String {
        let const_value = match constant {
            crate::Constant::Integer(integer) => integer.to_string(),
            crate::Constant::Boolean(boolean) => boolean.to_string(),
            crate::Constant::Unit => "()".to_string(),
        };
        format!("const {const_value}")
    }

    fn debug_operand(operand: &crate::Operand, body: &crate::Body) -> String {
        match operand {
            crate::Operand::Place(place) => debug_place(place, body),
            crate::Operand::Constant(constant) => debug_constant(constant),
        }
    }

    fn debug_termination(termination: &crate::Termination, body: &crate::Body) -> String {
        match termination {
            crate::Termination::Return(return_local_idx) => {
                let return_local = &body.locals[*return_local_idx];
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
    fn test_add_number() {
        check(
            r#"
                fn main() -> int {
                    10 + 20
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = add(const 10, const 20)
                        _0 = _1
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        check(
            r#"
                fn main() -> int {
                    let a = 100;
                    let b = 10 + 20;

                    10 + 20 + a + b
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int
                    let _2: int
                    let _3: int
                    let _4: int
                    let _5: int
                    let _6: int

                    entry: {
                        _1 = const 100
                        _3 = add(const 10, const 20)
                        _2 = _3
                        _4 = add(const 10, const 20)
                        _5 = add(_4, _1)
                        _6 = add(_5, _3)
                        _0 = _6
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
    fn test_sub_number() {
        check(
            r#"
                fn main() -> int {
                    10 - 20
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = sub(const 10, const 20)
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
    fn test_mul_number() {
        check(
            r#"
                fn main() -> int {
                    10 * 20
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = mul(const 10, const 20)
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
    fn test_div_number() {
        check(
            r#"
                fn main() -> int {
                    10 / 20
                }
            "#,
            expect![[r#"
                fn main() -> int {
                    let _0: int
                    let _1: int

                    entry: {
                        _1 = div(const 10, const 20)
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
    fn test_main_return_unit() {
        check(
            r#"
                fn main() {
                }
            "#,
            expect![[r#"
                fn main() -> () {
                    let _0: ()

                    entry: {
                        goto -> exit
                    }

                    exit: {
                        return _0
                    }
                }
            "#]],
        );

        check(
            r#"
                fn main() {
                    10;
                }
            "#,
            expect![[r#"
                fn main() -> () {
                    let _0: ()

                    entry: {
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
    fn test_block_resolves_to_tail() {
        check(
            r#"
                fn main() -> int {
                    {
                        let x = 10
                        x
                    }
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
    fn test_block_without_tail_resolves_to_unit() {
        check(
            r#"
                fn main() {
                    {
                        let x = 10
                    }
                }
            "#,
            expect![[r#"
                fn main() -> () {
                    let _0: ()
                    let _1: int

                    entry: {
                        _1 = const 10
                        _0 = const ()
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
