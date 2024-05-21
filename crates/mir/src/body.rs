use std::collections::HashMap;

use la_arena::{Arena, Idx};

use crate::{
    AggregateKind, AllocatedSwitchBB, BasicBlock, BinaryOp, Body, Constant, FunctionId, Local,
    LoweredExpr, LoweredStmt, Operand, Param, Place, PlaceKind, Projection, Statement, Termination,
    UnaryOp, Value,
};

struct BreakContext {
    continue_dest_block: Idx<BasicBlock>,
    break_dest_block: Idx<BasicBlock>,
    break_value: Idx<Local>,
}

pub(crate) struct FunctionLower<'a> {
    db: &'a dyn hir_ty::HirTyMasterDatabase,
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

    break_context_stack: Vec<BreakContext>,
}

impl<'a> FunctionLower<'a> {
    pub(crate) fn new(
        db: &'a dyn hir_ty::HirTyMasterDatabase,
        hir_file: hir::HirFile,
        resolution_map: &'a hir::ResolutionMap,
        hir_ty_result: &'a hir_ty::TyLowerResult,
        function_by_hir_function: &'a HashMap<hir::Function, FunctionId>,
        function: hir::Function,
    ) -> Self {
        let mut locals = Arena::new();
        let signature = hir_ty_result.signature_by_function(function).unwrap();
        let return_local = locals.alloc(Local {
            ty: signature.return_type(db),
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
            break_context_stack: vec![],
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
        assert!(current_bb.termination.is_none());
        current_bb.termination = Some(termination);
    }

    fn get_inference_by_function(&self, function: hir::Function) -> &hir_ty::InferenceBodyResult {
        self.hir_ty_result
            .inference_body_by_function(function)
            .unwrap()
    }

    fn alloc_local(&mut self, expr: hir::ExprId) -> Idx<Local> {
        let ty = self
            .get_inference_by_function(self.function)
            .type_by_expr(expr)
            .unwrap();
        let local_idx = self.alloc_local_by_ty(ty.clone());
        self.local_by_hir.insert(expr, local_idx);

        local_idx
    }

    fn alloc_local_by_ty(&mut self, ty: hir_ty::Monotype) -> Idx<Local> {
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
        assert!(self.exit_bb_idx.is_none());

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

    fn alloc_loop_bb(&mut self) -> Idx<BasicBlock> {
        let dest_bb = BasicBlock::new_loop_bb(self.block_idx);
        self.block_idx += 1;
        self.blocks.alloc(dest_bb)
    }

    fn lower_expr(&mut self, expr_id: hir::ExprId) -> LoweredExpr {
        let expr = expr_id.lookup(self.hir_file.db(self.db));
        match expr {
            hir::Expr::Symbol(symbol) => match symbol {
                hir::Symbol::Param { name: _, param } => {
                    LoweredExpr::Operand(Operand::Place(Place {
                        local: PlaceKind::Param(self.get_param_by_expr(*param)),
                        projection: None,
                    }))
                }
                hir::Symbol::Local { binding, .. } => {
                    let expr = binding.lookup(self.hir_file.db(self.db)).expr;
                    LoweredExpr::Operand(Operand::Place(Place {
                        local: PlaceKind::Local(self.get_local_by_expr(expr)),
                        projection: None,
                    }))
                }
                hir::Symbol::MissingExpr { .. } => todo!(),
                hir::Symbol::MissingType { .. } => unreachable!(),
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
                        LoweredExpr::Break => return LoweredExpr::Break,
                        LoweredExpr::Return => return LoweredExpr::Return,
                    };
                    let return_value_place = Place {
                        local: PlaceKind::Local(self.return_local),
                        projection: None,
                    };
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
                enum ControlKind {
                    Return,
                    Break,
                    None,
                }

                let cond_local_idx = {
                    let cond_local_idx = self.alloc_local(*condition);
                    let cond_operand = match self.lower_expr(*condition) {
                        LoweredExpr::Operand(operand) => operand,
                        LoweredExpr::Return => return LoweredExpr::Return,
                        LoweredExpr::Break => return LoweredExpr::Break,
                    };
                    let place = Place {
                        local: PlaceKind::Local(cond_local_idx),
                        projection: None,
                    };
                    self.add_statement_to_current_bb(Statement::Assign {
                        place,
                        value: cond_operand.into(),
                    });

                    cond_local_idx
                };

                let dest_bb_and_result_local_idx = self.alloc_dest_bb_and_result_local(expr_id);

                let switch_bb = self.alloc_switch_bb();
                self.add_termination_to_current_bb(Termination::Switch {
                    condition: Place {
                        local: PlaceKind::Local(cond_local_idx),
                        projection: None,
                    },
                    then_bb: switch_bb.then_bb_idx,
                    else_bb: switch_bb.else_bb_idx,
                });

                let mut control_in_then_branch = ControlKind::None;
                {
                    self.current_bb = Some(switch_bb.then_bb_idx);

                    let then_block = match then_branch.lookup(self.hir_file.db(self.db)) {
                        hir::Expr::Block(block) => block,
                        _ => unreachable!(),
                    };

                    for stmt in &then_block.stmts {
                        match self.lower_stmt(stmt) {
                            LoweredStmt::Return => {
                                control_in_then_branch = ControlKind::Return;
                                break;
                            }
                            LoweredStmt::Break => {
                                control_in_then_branch = ControlKind::Break;
                                break;
                            }
                            LoweredStmt::Unit => (),
                        }
                    }
                    if matches!(control_in_then_branch, ControlKind::None) {
                        if let Some(tail) = then_block.tail {
                            match self.lower_expr(tail) {
                                LoweredExpr::Operand(operand) => {
                                    self.add_statement_to_current_bb(Statement::Assign {
                                        place: Place {
                                            local: PlaceKind::Local(dest_bb_and_result_local_idx.1),
                                            projection: None,
                                        },
                                        value: operand.into(),
                                    });
                                    self.add_termination_to_current_bb(Termination::Goto(
                                        dest_bb_and_result_local_idx.0,
                                    ));
                                }
                                LoweredExpr::Return => {
                                    control_in_then_branch = ControlKind::Return;
                                }
                                LoweredExpr::Break => {
                                    control_in_then_branch = ControlKind::Break;
                                }
                            };
                        } else {
                            self.add_termination_to_current_bb(Termination::Goto(
                                dest_bb_and_result_local_idx.0,
                            ));
                        }
                    }
                }

                let mut control_in_else_branch = ControlKind::None;
                {
                    self.current_bb = Some(switch_bb.else_bb_idx);

                    match else_branch {
                        Some(else_block_expr) => {
                            let else_block = match else_block_expr.lookup(self.hir_file.db(self.db))
                            {
                                hir::Expr::Block(block) => block,
                                _ => unreachable!(),
                            };

                            for stmt in &else_block.stmts {
                                match self.lower_stmt(stmt) {
                                    LoweredStmt::Return => {
                                        control_in_else_branch = ControlKind::Return;
                                        break;
                                    }
                                    LoweredStmt::Break => {
                                        control_in_else_branch = ControlKind::Break;
                                        break;
                                    }
                                    LoweredStmt::Unit => (),
                                }
                            }

                            if matches!(control_in_else_branch, ControlKind::None) {
                                if let Some(tail) = else_block.tail {
                                    match self.lower_expr(tail) {
                                        LoweredExpr::Operand(operand) => {
                                            self.add_statement_to_current_bb(Statement::Assign {
                                                place: Place {
                                                    local: PlaceKind::Local(
                                                        dest_bb_and_result_local_idx.1,
                                                    ),
                                                    projection: None,
                                                },
                                                value: operand.into(),
                                            });
                                            self.add_termination_to_current_bb(Termination::Goto(
                                                dest_bb_and_result_local_idx.0,
                                            ));
                                        }
                                        LoweredExpr::Return => {
                                            control_in_then_branch = ControlKind::Return;
                                        }
                                        LoweredExpr::Break => {
                                            control_in_then_branch = ControlKind::Break;
                                        }
                                    };
                                } else {
                                    self.add_termination_to_current_bb(Termination::Goto(
                                        dest_bb_and_result_local_idx.0,
                                    ));
                                }
                            }
                        }
                        None => {
                            let unit = Operand::Constant(Constant::Unit);
                            self.add_statement_to_current_bb(Statement::Assign {
                                place: Place {
                                    local: PlaceKind::Local(dest_bb_and_result_local_idx.1),
                                    projection: None,
                                },
                                value: unit.into(),
                            });

                            self.add_termination_to_current_bb(Termination::Goto(
                                dest_bb_and_result_local_idx.0,
                            ));
                        }
                    }
                }

                self.current_bb = Some(dest_bb_and_result_local_idx.0);
                match (control_in_then_branch, control_in_else_branch) {
                    (ControlKind::Return, ControlKind::Return) => LoweredExpr::Return,

                    (ControlKind::Break, ControlKind::Break)
                    | (ControlKind::Return, ControlKind::Break)
                    | (ControlKind::Break, ControlKind::Return) => LoweredExpr::Break,

                    (ControlKind::None, _) | (_, ControlKind::None) => {
                        LoweredExpr::Operand(Operand::Place(Place {
                            local: PlaceKind::Local(dest_bb_and_result_local_idx.1),
                            projection: None,
                        }))
                    }
                }
            }
            hir::Expr::Binary { op, lhs, rhs } => {
                let lhs_operand = match self.lower_expr(*lhs) {
                    LoweredExpr::Return => return LoweredExpr::Return,
                    LoweredExpr::Break => return LoweredExpr::Break,
                    LoweredExpr::Operand(operand) => operand,
                };

                let rhs_operand = match self.lower_expr(*rhs) {
                    LoweredExpr::Return => return LoweredExpr::Return,
                    LoweredExpr::Break => return LoweredExpr::Break,
                    LoweredExpr::Operand(operand) => operand,
                };

                let op = match op {
                    hir::BinaryOp::Add => BinaryOp::Add,
                    hir::BinaryOp::Sub => BinaryOp::Sub,
                    hir::BinaryOp::Mul => BinaryOp::Mul,
                    hir::BinaryOp::Div => BinaryOp::Div,
                    hir::BinaryOp::Equal => BinaryOp::Equal,
                    hir::BinaryOp::NotEq => BinaryOp::NotEq,
                    hir::BinaryOp::GreaterThan => BinaryOp::GreaterThan,
                    hir::BinaryOp::LessThan => BinaryOp::LessThan,
                    hir::BinaryOp::GtEq => BinaryOp::GtEq,
                    hir::BinaryOp::LtEq => BinaryOp::LtEq,
                    hir::BinaryOp::Assign => {
                        let place = match lhs_operand {
                            Operand::Place(place) => place,
                            Operand::Constant(_) => unreachable!(),
                        };
                        let value = Value::Operand(rhs_operand);
                        self.add_statement_to_current_bb(Statement::Assign { place, value });
                        return LoweredExpr::Operand(Operand::Constant(Constant::Unit));
                    }
                };

                let local = self.alloc_local(expr_id);
                let value = Value::BinaryOp {
                    op,
                    left: lhs_operand,
                    right: rhs_operand,
                };
                let place = Place {
                    local: PlaceKind::Local(local),
                    projection: None,
                };
                self.add_statement_to_current_bb(Statement::Assign { place, value });

                LoweredExpr::Operand(Operand::Place(place))
            }
            hir::Expr::Unary { op, expr } => {
                let expr = match self.lower_expr(*expr) {
                    LoweredExpr::Return => return LoweredExpr::Return,
                    LoweredExpr::Break => return LoweredExpr::Break,
                    LoweredExpr::Operand(operand) => operand,
                };
                let op = match op {
                    hir::UnaryOp::Neg => UnaryOp::Neg,
                    hir::UnaryOp::Not => UnaryOp::Not,
                };
                let local = self.alloc_local(expr_id);
                let value = Value::UnaryOp { op, expr };
                let place = Place {
                    local: PlaceKind::Local(local),
                    projection: None,
                };
                self.add_statement_to_current_bb(Statement::Assign { place, value });

                LoweredExpr::Operand(Operand::Place(place))
            }
            hir::Expr::Block(block) => {
                for stmt in &block.stmts {
                    match self.lower_stmt(stmt) {
                        LoweredStmt::Return => return LoweredExpr::Return,
                        LoweredStmt::Break => return LoweredExpr::Break,
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
                        LoweredExpr::Break => return LoweredExpr::Break,
                        LoweredExpr::Operand(operand) => {
                            arg_operands.push(operand);
                        }
                    }
                }

                match callee {
                    hir::Symbol::Param { .. } => unimplemented!(),
                    hir::Symbol::Local { .. } => unimplemented!(),
                    hir::Symbol::MissingType { .. } => unreachable!(),
                    hir::Symbol::MissingExpr { path } => {
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

                                let signature =
                                    self.hir_ty_result.signature_by_function(function).unwrap();
                                let called_local =
                                    self.alloc_local_by_ty(signature.return_type(self.db));
                                let dest_place = Place {
                                    local: PlaceKind::Local(called_local),
                                    projection: None,
                                };

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
                            hir::Item::Struct(struct_) => {
                                let mut field_operands = vec![];
                                for field in args {
                                    let field_operand = match self.lower_expr(*field) {
                                        LoweredExpr::Return => return LoweredExpr::Return,
                                        LoweredExpr::Break => return LoweredExpr::Break,
                                        LoweredExpr::Operand(operand) => operand,
                                    };
                                    field_operands.push(field_operand);
                                }

                                let local = self.alloc_local(expr_id);
                                let value = Value::Aggregate {
                                    kind: AggregateKind::Struct(struct_),
                                    operands: field_operands,
                                };
                                let place = Place {
                                    local: PlaceKind::Local(local),
                                    projection: None,
                                };
                                self.add_statement_to_current_bb(Statement::Assign {
                                    place,
                                    value,
                                });

                                LoweredExpr::Operand(Operand::Place(place))
                            }
                            hir::Item::Module(_) | hir::Item::UseItem(_) => unimplemented!(),
                        }
                    }
                }
            }
            hir::Expr::Loop { block } => {
                let loop_block = self.alloc_loop_bb();
                self.add_termination_to_current_bb(Termination::Goto(loop_block));
                self.current_bb = Some(loop_block);

                // break値の型を参照したいので、loop中のブロック式の型ではなくloop式自体の型を使う
                let (break_dest_block, break_value) = self.alloc_dest_bb_and_result_local(expr_id);

                self.enter_break_context(BreakContext {
                    continue_dest_block: loop_block,
                    break_dest_block,
                    break_value,
                });

                let lowered = self.lower_expr(*block);
                match lowered {
                    LoweredExpr::Return => {
                        self.exit_break_context();
                        self.current_bb = Some(break_dest_block);
                        LoweredExpr::Return
                    }
                    LoweredExpr::Break => {
                        self.exit_break_context();
                        self.current_bb = Some(break_dest_block);
                        LoweredExpr::Operand(Operand::Place(Place {
                            local: PlaceKind::Local(break_value),
                            projection: None,
                        }))
                    }
                    LoweredExpr::Operand(_) => {
                        self.add_termination_to_current_bb(Termination::Goto(loop_block));
                        self.exit_break_context();
                        self.current_bb = Some(break_dest_block);
                        LoweredExpr::Operand(Operand::Place(Place {
                            local: PlaceKind::Local(break_value),
                            projection: None,
                        }))
                    }
                }
            }
            hir::Expr::Continue => {
                self.add_termination_to_current_bb(Termination::Goto(
                    self.current_break_context().continue_dest_block,
                ));

                LoweredExpr::Break
            }
            hir::Expr::Break { value } => {
                if let Some(value) = value {
                    let value = self.lower_expr(*value);
                    match value {
                        LoweredExpr::Return => return LoweredExpr::Return,
                        LoweredExpr::Break => return LoweredExpr::Break,
                        LoweredExpr::Operand(operand) => {
                            self.add_statement_to_current_bb(Statement::Assign {
                                place: Place {
                                    local: PlaceKind::Local(
                                        self.current_break_context().break_value,
                                    ),
                                    projection: None,
                                },
                                value: Value::Operand(operand),
                            });
                        }
                    }
                } else {
                    self.add_statement_to_current_bb(Statement::Assign {
                        place: Place {
                            local: PlaceKind::Local(self.current_break_context().break_value),
                            projection: None,
                        },
                        value: Value::Operand(Operand::Constant(Constant::Unit)),
                    });
                }

                self.add_termination_to_current_bb(Termination::Goto(
                    self.current_break_context().break_dest_block,
                ));

                LoweredExpr::Break
            }
            hir::Expr::Record { symbol, fields } => {
                let struct_ = match symbol {
                    hir::Symbol::MissingExpr { path } => {
                        let resolution_status = self.resolution_map.item_by_symbol(path).unwrap();
                        let item = match resolution_status {
                            hir::ResolutionStatus::Unresolved | hir::ResolutionStatus::Error => {
                                unreachable!();
                            }
                            hir::ResolutionStatus::Resolved { path: _, item } => item,
                        };
                        match item {
                            hir::Item::Struct(struct_) => struct_,
                            hir::Item::Function(_)
                            | hir::Item::Module(_)
                            | hir::Item::UseItem(_) => unreachable!(),
                        }
                    }
                    hir::Symbol::Param { .. }
                    | hir::Symbol::Local { .. }
                    | hir::Symbol::MissingType { .. } => unreachable!(),
                };

                match struct_.kind(self.db) {
                    hir::StructKind::Unit | hir::StructKind::Tuple(_) => unreachable!(),
                    hir::StructKind::Record(defined_fields) => {
                        // コード上の順番を維持する
                        let mut operand_by_name = HashMap::new();
                        for field in fields {
                            let lowerd_expr = self.lower_expr(field.value);
                            let operand = match lowerd_expr {
                                LoweredExpr::Operand(operand) => operand,
                                LoweredExpr::Return | LoweredExpr::Break => return lowerd_expr,
                            };

                            operand_by_name.insert(field.name, operand);
                        }

                        let mut operands = vec![];
                        for defined_field in defined_fields {
                            let operand = operand_by_name.remove(&defined_field.name).unwrap();
                            operands.push(operand);
                        }

                        let local = self.alloc_local(expr_id);
                        let value = Value::Aggregate {
                            kind: AggregateKind::Struct(struct_),
                            operands,
                        };
                        let place = Place {
                            local: PlaceKind::Local(local),
                            projection: None,
                        };
                        self.add_statement_to_current_bb(Statement::Assign { place, value });

                        LoweredExpr::Operand(Operand::Place(place))
                    }
                }
            }
            hir::Expr::Field {
                base: base_expr_id,
                name,
            } => {
                let base = match self.lower_expr(*base_expr_id) {
                    LoweredExpr::Operand(operand) => operand,
                    LoweredExpr::Return => return LoweredExpr::Return,
                    LoweredExpr::Break => return LoweredExpr::Break,
                };

                let local_base = self.alloc_local(*base_expr_id);
                self.add_statement_to_current_bb(Statement::Assign {
                    place: Place {
                        local: PlaceKind::Local(local_base),
                        projection: None,
                    },
                    value: base.into(),
                });

                let hir_ty::Monotype::Struct(struct_) = self
                    .get_inference_by_function(self.function)
                    .type_by_expr(*base_expr_id)
                    .unwrap()
                else {
                    unreachable!();
                };

                match struct_.kind(self.db) {
                    hir::StructKind::Unit => unimplemented!(),
                    hir::StructKind::Tuple(_) => {
                        let idx = name.text(self.db).parse::<usize>().unwrap();
                        let projection = Projection::Field { idx, name: *name };
                        let place = Place {
                            local: PlaceKind::Local(local_base),
                            projection: Some(projection),
                        };

                        LoweredExpr::Operand(Operand::Place(place))
                    }
                    hir::StructKind::Record(fields) => {
                        let idx = fields.iter().position(|field| field.name == *name).unwrap();
                        let projection = Projection::Field { idx, name: *name };
                        let place = Place {
                            local: PlaceKind::Local(local_base),
                            projection: Some(projection),
                        };

                        LoweredExpr::Operand(Operand::Place(place))
                    }
                }
            }
            hir::Expr::Missing => unreachable!(),
        }
    }

    fn lower_stmt(&mut self, stmt: &hir::Stmt) -> LoweredStmt {
        match stmt {
            hir::Stmt::Let { value, .. } => {
                let local_idx = self.alloc_local(*value);
                let operand = match self.lower_expr(*value) {
                    LoweredExpr::Operand(operand) => operand,
                    LoweredExpr::Return => {
                        return LoweredStmt::Return;
                    }
                    LoweredExpr::Break => return LoweredStmt::Break,
                };
                self.add_statement_to_current_bb(Statement::Assign {
                    place: Place {
                        local: PlaceKind::Local(local_idx),
                        projection: None,
                    },
                    value: operand.into(),
                });
            }
            hir::Stmt::Expr { expr, .. } => {
                match self.lower_expr(*expr) {
                    LoweredExpr::Operand(operand) => operand,
                    LoweredExpr::Return => {
                        return LoweredStmt::Return;
                    }
                    LoweredExpr::Break => return LoweredStmt::Break,
                };
            }
            hir::Stmt::Item { item } => match item {
                hir::Item::Function(_) => unreachable!(),
                hir::Item::Struct(_) | hir::Item::Module(_) | hir::Item::UseItem(_) => {
                    return LoweredStmt::Unit;
                }
            },
        }

        LoweredStmt::Unit
    }

    pub(crate) fn lower(mut self) -> Body {
        let signature = self
            .hir_ty_result
            .signature_by_function(self.function)
            .unwrap();
        for (param, param_ty) in self
            .function
            .params(self.db)
            .iter()
            .zip(signature.params(self.db).iter())
        {
            let param_idx = self.params.alloc(Param {
                ty: param_ty.clone(),
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

        let body_block = self.function.body(self.db, self.hir_file).unwrap();
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
                LoweredStmt::Break => unreachable!(),
                LoweredStmt::Unit => (),
            }
        }

        if !has_return {
            if let Some(tail) = body_block.tail {
                match self.lower_expr(tail) {
                    LoweredExpr::Operand(operand) => {
                        self.add_statement_to_current_bb(Statement::Assign {
                            place: Place {
                                local: PlaceKind::Local(self.return_local),
                                projection: None,
                            },
                            value: operand.into(),
                        });
                        self.add_termination_to_current_bb(Termination::Goto(exit_bb_idx));
                        self.current_bb = Some(exit_bb_idx);
                    }
                    LoweredExpr::Break => unreachable!(),
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

    fn enter_break_context(&mut self, break_context: BreakContext) {
        self.break_context_stack.push(break_context);
    }

    fn exit_break_context(&mut self) {
        assert!(!self.break_context_stack.is_empty());
        self.break_context_stack.pop();
    }

    fn current_break_context(&self) -> &BreakContext {
        self.break_context_stack.last().unwrap()
    }
}
