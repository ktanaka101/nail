use std::collections::HashMap;

use la_arena::{Arena, Idx};

use crate::{
    AllocatedSwitchBB, BasicBlock, BinaryOp, Body, Constant, FunctionId, Local, LoweredExpr,
    LoweredStmt, Operand, Param, Place, Statement, Termination, UnaryOp, Value,
};

pub(crate) struct FunctionLower<'a> {
    db: &'a dyn hir::HirMasterDatabase,
    resolution_map: &'a hir::ResolutionMap,
    hir_ty_result: &'a hir_ty::TyLowerResult,
    function_by_hir_function: &'a HashMap<hir::Function, FunctionId>,

    hir_file: hir::HirFile,
    function: hir::Function,

    return_local: Idx<Local>,
    params: Arena<Param>,
    param_by_hir: HashMap<hir::Param, Idx<Param>>,
    locals: Arena<Local>,
    local_idx: u64,
    blocks: Arena<BasicBlock>,
    switch_idx: u64,
    current_bb: Option<Idx<BasicBlock>>,
    block_idx: u64,
    local_by_hir: HashMap<hir::ExprId, Idx<Local>>,
    exit_bb_idx: Option<Idx<BasicBlock>>,
}

impl<'a> FunctionLower<'a> {
    pub(crate) fn new(
        db: &'a dyn hir::HirMasterDatabase,
        hir_file: hir::HirFile,
        resolution_map: &'a hir::ResolutionMap,
        hir_ty_result: &'a hir_ty::TyLowerResult,
        function_by_hir_function: &'a HashMap<hir::Function, FunctionId>,
        function: hir::Function,
    ) -> Self {
        let mut locals = Arena::new();
        let signature = &hir_ty_result.signature_by_function(function);
        let return_local = locals.alloc(Local {
            ty: signature.return_type,
            idx: 0,
        });

        FunctionLower {
            db,
            hir_file,
            resolution_map,
            hir_ty_result,
            function_by_hir_function,
            function,
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

    fn get_inference_by_function(&self, function: hir::Function) -> &hir_ty::InferenceBodyResult {
        self.hir_ty_result
            .inference_result
            .inference_by_body
            .get(&function)
            .unwrap()
    }

    fn alloc_local(&mut self, expr: hir::ExprId) -> Idx<Local> {
        let ty = self.get_inference_by_function(self.function).type_by_expr[&expr];
        let local_idx = self.alloc_local_by_ty(ty);
        self.local_by_hir.insert(expr, local_idx);

        local_idx
    }

    fn alloc_local_by_ty(&mut self, ty: hir_ty::ResolvedType) -> Idx<Local> {
        let local = Local {
            ty,
            idx: self.local_idx,
        };
        self.local_idx += 1;

        self.locals.alloc(local)
    }

    fn get_local_by_expr(&self, expr: hir::ExprId) -> Idx<Local> {
        *self.local_by_hir.get(&expr).unwrap()
    }

    fn get_param_by_expr(&self, param: hir::Param) -> Idx<Param> {
        self.param_by_hir[&param]
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
        expr: hir::ExprId,
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

    fn lower_expr(&mut self, expr_id: hir::ExprId) -> LoweredExpr {
        let expr = expr_id.lookup(self.hir_file.db(self.db));
        match expr {
            hir::Expr::Symbol(symbol) => match symbol {
                hir::Symbol::Param { name: _, param } => LoweredExpr::Operand(Operand::Place(
                    Place::Param(self.get_param_by_expr(*param)),
                )),
                hir::Symbol::Local { name: _, expr } => LoweredExpr::Operand(Operand::Place(
                    Place::Local(self.get_local_by_expr(*expr)),
                )),
                hir::Symbol::Missing { .. } => todo!(),
            },
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Integer(value) => {
                    LoweredExpr::Operand(Operand::Constant(Constant::Integer(*value)))
                }
                hir::Literal::Bool(value) => {
                    LoweredExpr::Operand(Operand::Constant(Constant::Boolean(*value)))
                }
                hir::Literal::String(string) => {
                    LoweredExpr::Operand(Operand::Constant(Constant::String(string.to_owned())))
                }
                _ => todo!(),
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

                self.add_termination_to_current_bb(Termination::Goto(self.exit_bb_idx()));
                self.current_bb = Some(self.exit_bb_idx());

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

                    let then_block = match then_branch.lookup(self.hir_file.db(self.db)) {
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
                                LoweredExpr::Return => (),
                            };
                        }
                    }
                }

                {
                    self.current_bb = Some(switch_bb.else_bb_idx);

                    match else_branch {
                        Some(else_block) => {
                            let else_block = match else_block.lookup(self.hir_file.db(self.db)) {
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
                                        LoweredExpr::Return => (),
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
                    ast::BinaryOp::Add(_) => BinaryOp::Add,
                    ast::BinaryOp::Sub(_) => BinaryOp::Sub,
                    ast::BinaryOp::Mul(_) => BinaryOp::Mul,
                    ast::BinaryOp::Div(_) => BinaryOp::Div,
                    ast::BinaryOp::Equal(_) => BinaryOp::Equal,
                    ast::BinaryOp::GreaterThan(_) => BinaryOp::GreaterThan,
                    ast::BinaryOp::LessThan(_) => BinaryOp::LessThan,
                };

                let local = self.alloc_local(expr_id);
                let value = Value::BinaryOp {
                    op,
                    left: lhs,
                    right: rhs,
                };
                let place = Place::Local(local);
                self.add_statement_to_current_bb(Statement::Assign { place, value });

                LoweredExpr::Operand(Operand::Place(place))
            }
            hir::Expr::Unary { op, expr } => {
                let expr = match self.lower_expr(*expr) {
                    LoweredExpr::Return => return LoweredExpr::Return,
                    LoweredExpr::Operand(operand) => operand,
                };
                let op = match op {
                    ast::UnaryOp::Neg(_) => UnaryOp::Neg,
                    ast::UnaryOp::Not(_) => UnaryOp::Not,
                };
                let local = self.alloc_local(expr_id);
                let value = Value::UnaryOp { op, expr };
                let place = Place::Local(local);
                self.add_statement_to_current_bb(Statement::Assign { place, value });

                LoweredExpr::Operand(Operand::Place(place))
            }
            hir::Expr::Block(block) => {
                for stmt in &block.stmts {
                    match self.lower_stmt(stmt) {
                        LoweredStmt::Return => return LoweredExpr::Return,
                        LoweredStmt::Unit => (),
                    }
                }

                if let Some(tail) = block.tail {
                    self.lower_expr(tail)
                } else {
                    LoweredExpr::Operand(Operand::Constant(Constant::Unit))
                }
            }
            hir::Expr::Call { callee, args } => {
                let mut arg_operands = vec![];
                for arg in args {
                    match self.lower_expr(*arg) {
                        LoweredExpr::Return => return LoweredExpr::Return,
                        LoweredExpr::Operand(operand) => {
                            arg_operands.push(operand);
                        }
                    }
                }

                match callee {
                    hir::Symbol::Param { .. } => unimplemented!(),
                    hir::Symbol::Local { .. } => unimplemented!(),
                    hir::Symbol::Missing { path } => {
                        let resolution_status = self.resolution_map.item_by_symbol(path).unwrap();
                        let item = match resolution_status {
                            hir::ResolutionStatus::Unresolved | hir::ResolutionStatus::Error => {
                                unimplemented!()
                            }
                            hir::ResolutionStatus::Resolved { path: _, item } => item,
                        };
                        match item {
                            hir::Item::Function(function) => {
                                let function_id = self.function_by_hir_function[&function];

                                let signature = self.hir_ty_result.signature_by_function(function);
                                let called_local = self.alloc_local_by_ty(signature.return_type);
                                let dest_place = Place::Local(called_local);

                                let target_bb = self.alloc_standard_bb();

                                self.add_termination_to_current_bb(Termination::Call {
                                    function: function_id,
                                    args: arg_operands,
                                    destination: dest_place,
                                    target: target_bb,
                                });
                                self.current_bb = Some(target_bb);

                                LoweredExpr::Operand(Operand::Place(dest_place))
                            }
                            hir::Item::Module(_) | hir::Item::UseItem(_) => unimplemented!(),
                        }
                    }
                }
            }
            hir::Expr::Missing => unreachable!(),
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
            hir::Stmt::Item { item } => match item {
                hir::Item::Function(_) => unreachable!(),
                hir::Item::Module(_) | hir::Item::UseItem(_) => {
                    return LoweredStmt::Unit;
                }
            },
        }

        LoweredStmt::Unit
    }

    pub(crate) fn lower(mut self) -> Body {
        let signature = self.hir_ty_result.signature_by_function(self.function);
        for (param, param_ty) in self
            .function
            .params(self.db)
            .iter()
            .zip(signature.params.iter())
        {
            let param_idx = self.params.alloc(Param {
                ty: *param_ty,
                idx: self.local_idx,
                pos: param
                    .data(self.hir_file.db(self.db))
                    .pos
                    .try_into()
                    .unwrap(),
            });
            self.param_by_hir.insert(*param, param_idx);

            self.local_idx += 1;
        }

        let body_block = self
            .hir_file
            .function_body_by_function(self.db, self.function)
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
        }

        Body {
            path: self.resolution_map.path_of_function(self.function).unwrap(),
            name: self.function.name(self.db),
            params: self.params,
            return_local: self.return_local,
            locals: self.locals,
            blocks: self.blocks,
        }
    }
}