use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    types::BasicTypeEnum,
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace,
};

use crate::Codegen;

/// BodyCodegen: MIRをもとにIRを生成する
pub(crate) struct BodyCodegen<'a, 'ctx> {
    codegen: &'a Codegen<'a, 'ctx>,

    function_id: mir::FunctionId,
    function: FunctionValue<'ctx>,
    body: &'a mir::Body,

    /// ローカル変数を保持: (型, allocaされたポインタ)
    /// ただし "struct" 場合は "pointer to struct" を持つ
    locals: HashMap<mir::LocalIdx, (BasicTypeEnum<'ctx>, PointerValue<'ctx>)>,
    basic_blocks: HashMap<mir::BasicBlockIdx, BasicBlock<'ctx>>,
}

impl<'a, 'ctx> BodyCodegen<'a, 'ctx> {
    pub(crate) fn new(
        codegen: &'a Codegen<'a, 'ctx>,
        function_id: mir::FunctionId,
        function: FunctionValue<'ctx>,
        body: &'a mir::Body,
    ) -> Self {
        Self {
            codegen,
            function_id,
            function,
            body,
            locals: HashMap::new(),
            basic_blocks: HashMap::new(),
        }
    }

    pub(crate) fn gen(mut self) {
        self.codegen.move_to_entry_bb_in_function(self.function_id);

        for (idx, local) in self.body.locals.iter() {
            let ty: BasicTypeEnum = match local.ty {
                hir_ty::Monotype::Unit => self.codegen.unit_type().into(),
                hir_ty::Monotype::Integer => self.codegen.integer_type().into(),
                hir_ty::Monotype::String => self.codegen.string_type().into(),
                hir_ty::Monotype::Bool => self.codegen.bool_type().into(),
                hir_ty::Monotype::Char => todo!("char not yet handled"),
                hir_ty::Monotype::Struct(_) => self
                    .codegen
                    .context
                    .ptr_type(AddressSpace::default())
                    .into(),
                hir_ty::Monotype::Never => continue, // unreachable
                hir_ty::Monotype::Function(_) => todo!("functions not yet handled"),
                hir_ty::Monotype::Variable(_) | hir_ty::Monotype::Unknown => unreachable!(),
            };

            let local_ptr = self
                .codegen
                .builder
                .build_alloca(ty, &local.idx.to_string())
                .unwrap();
            self.locals.insert(idx, (ty, local_ptr));
        }

        // register basic blocks
        let mut is_entry = true;
        for (bb_idx, bb) in self.body.blocks.iter() {
            let appended_bb = self
                .codegen
                .context
                .append_basic_block(self.function, &bb.name());
            self.basic_blocks.insert(bb_idx, appended_bb);

            if is_entry {
                is_entry = false;
                self.codegen
                    .builder
                    .build_unconditional_branch(appended_bb)
                    .unwrap();
            }
        }

        // codegen
        for (bb_idx, bb) in self.body.blocks.iter() {
            self.move_to_bb_by_bb_idx(bb_idx);
            self.gen_bb(bb);
        }
    }

    fn move_to_bb_by_bb_idx(&self, bb_idx: mir::BasicBlockIdx) {
        let target_bb = self.basic_blocks.get(&bb_idx).unwrap();
        self.codegen.builder.position_at_end(*target_bb);
    }

    fn store_local(&self, local_idx: mir::LocalIdx, value: BasicValueEnum<'ctx>) {
        let (_ty, alloca_ptr) = self.locals[&local_idx];
        self.codegen.builder.build_store(alloca_ptr, value).unwrap();
    }

    fn load_local(&self, local_idx: mir::LocalIdx) -> BasicValueEnum<'ctx> {
        let (ty, alloca_ptr) = self.locals[&local_idx];
        self.codegen
            .builder
            .build_load(ty, alloca_ptr, "load_local")
            .unwrap()
    }

    fn store_local_struct_gep(
        &self,
        local_idx: mir::LocalIdx,
        idx: u32,
        value: BasicValueEnum<'ctx>,
    ) {
        // 1) load pointer to struct
        let (ty, alloca_ptr) = self.locals[&local_idx];
        let struct_ty = {
            let t = self.body.locals[local_idx].ty;
            let struct_ = match t {
                hir_ty::Monotype::Struct(s) => s,
                _ => unreachable!(),
            };
            self.codegen.declaration_structs[&struct_]
        };
        let ptr_val = self
            .codegen
            .builder
            .build_load(ty, alloca_ptr, "load_struct_ptr")
            .unwrap()
            .into_pointer_value();

        // 2) GEP
        let field_ptr = self
            .codegen
            .builder
            .build_struct_gep(struct_ty, ptr_val, idx, "field_ptr")
            .unwrap();

        self.codegen.builder.build_store(field_ptr, value).unwrap();
    }

    fn load_local_struct_gep(
        &self,
        local_idx: mir::LocalIdx,
        idx: u32,
        field_ty: hir_ty::Monotype,
    ) -> BasicValueEnum<'ctx> {
        // 1) load pointer to struct
        let (ty, alloca_ptr) = self.locals[&local_idx];
        let ptr_val = self
            .codegen
            .builder
            .build_load(ty, alloca_ptr, "load_struct_ptr")
            .unwrap()
            .into_pointer_value();

        // 2) GEP
        let struct_ty = {
            let t = self.body.locals[local_idx].ty;
            let struct_ = match t {
                hir_ty::Monotype::Struct(s) => s,
                _ => unreachable!(),
            };
            self.codegen.declaration_structs[&struct_]
        };

        let field_ptr = self
            .codegen
            .builder
            .build_struct_gep(struct_ty, ptr_val, idx, "gep_field")
            .unwrap();

        let field_ty: BasicTypeEnum = match field_ty {
            hir_ty::Monotype::Unit => self.codegen.unit_type().into(),
            hir_ty::Monotype::Integer => self.codegen.integer_type().into(),
            hir_ty::Monotype::String => self.codegen.string_type().into(),
            hir_ty::Monotype::Bool => self.codegen.bool_type().into(),
            hir_ty::Monotype::Char => todo!("char not yet handled"),
            hir_ty::Monotype::Struct(_) => self
                .codegen
                .context
                .ptr_type(AddressSpace::default())
                .into(),
            hir_ty::Monotype::Never => unreachable!(),
            hir_ty::Monotype::Function(_) => todo!("functions not yet handled"),
            hir_ty::Monotype::Variable(_) | hir_ty::Monotype::Unknown => unreachable!(),
        };

        self.codegen
            .builder
            .build_load(field_ty, field_ptr, "load_field")
            .unwrap()
    }

    fn load_param(&self, param_idx: mir::ParamIdx) -> BasicValueEnum<'ctx> {
        let param = &self.body.params[param_idx];
        self.function.get_nth_param(param.pos).unwrap()
    }

    fn load_param_struct_gep(
        &self,
        param_idx: mir::ParamIdx,
        idx: u32,
        field_ty: hir_ty::Monotype,
    ) -> BasicValueEnum<'ctx> {
        let param = &self.body.params[param_idx];
        let param_val = self.function.get_nth_param(param.pos).unwrap();

        let struct_ptr = param_val.into_pointer_value();

        let struct_ty = {
            let struct_ = match param.ty {
                hir_ty::Monotype::Struct(s) => s,
                _ => unreachable!(),
            };
            self.codegen.declaration_structs[&struct_]
        };

        let field_ptr = self
            .codegen
            .builder
            .build_struct_gep(struct_ty, struct_ptr, idx, "param_field")
            .unwrap();

        let field_ty: BasicTypeEnum = match field_ty {
            hir_ty::Monotype::Unit => self.codegen.unit_type().into(),
            hir_ty::Monotype::Integer => self.codegen.integer_type().into(),
            hir_ty::Monotype::String => self.codegen.string_type().into(),
            hir_ty::Monotype::Bool => self.codegen.bool_type().into(),
            hir_ty::Monotype::Char => todo!("char not yet handled"),
            hir_ty::Monotype::Struct(_) => self
                .codegen
                .context
                .ptr_type(AddressSpace::default())
                .into(),
            hir_ty::Monotype::Never => unreachable!(),
            hir_ty::Monotype::Function(_) => todo!("functions not yet handled"),
            hir_ty::Monotype::Variable(_) | hir_ty::Monotype::Unknown => unreachable!(),
        };

        self.codegen
            .builder
            .build_load(field_ty, field_ptr, "load_param_field")
            .unwrap()
    }

    fn gen_bb(&self, bb: &mir::BasicBlock) {
        for stmt in &bb.statements {
            match stmt {
                mir::Statement::Assign { place, value } => {
                    let val = match value {
                        mir::Value::Operand(operand) => self.gen_operand(operand),
                        mir::Value::BinaryOp { op, left, right } => {
                            let lhs = self.gen_operand(left).into_int_value();
                            let rhs = self.gen_operand(right).into_int_value();
                            self.gen_int_binary_op(op, lhs, rhs).into()
                        }
                        mir::Value::UnaryOp { op, expr } => {
                            let expr_iv = self.gen_operand(expr).into_int_value();
                            self.gen_int_unary_op(op, expr_iv).into()
                        }
                        mir::Value::Aggregate { kind, operands } => match kind {
                            mir::AggregateKind::Struct(struct_) => {
                                let struct_ty = self.codegen.declaration_structs[struct_];

                                // 1) allocate struct on heap
                                let struct_ptr = self
                                    .codegen
                                    .build_call_gc_malloc(struct_ty.size_of().unwrap())
                                    .unwrap();

                                // 2) store each field
                                for (i, operand) in operands.iter().enumerate() {
                                    let field_val = self.gen_operand(operand);
                                    let field_ptr = self
                                        .codegen
                                        .builder
                                        .build_struct_gep(
                                            struct_ty,
                                            struct_ptr,
                                            i as u32,
                                            "struct_field",
                                        )
                                        .unwrap();
                                    self.codegen
                                        .builder
                                        .build_store(field_ptr, field_val)
                                        .unwrap();
                                }

                                // 3) return the pointer
                                struct_ptr.into()
                            }
                        },
                    };

                    // store to place
                    match place.local {
                        mir::PlaceKind::Param(_) => unreachable!(),
                        mir::PlaceKind::Local(local_idx) => {
                            if let Some(projection) = place.projection {
                                // if place is local.field
                                match projection {
                                    mir::Projection::Field { idx, .. } => {
                                        self.store_local_struct_gep(local_idx, idx, val);
                                    }
                                }
                            } else {
                                // plain local
                                self.store_local(local_idx, val);
                            }
                        }
                    }
                }
            }
        }

        // termination
        if let Some(termination) = &bb.termination {
            match termination {
                mir::Termination::Return(ret_local) => {
                    // load pointer or i64 etc. from ret_local
                    let ret_val = self.load_local(*ret_local);
                    self.codegen.builder.build_return(Some(&ret_val)).unwrap();
                }
                mir::Termination::Goto(goto_bb) => {
                    let dest_block = self.basic_blocks[goto_bb];
                    self.codegen
                        .builder
                        .build_unconditional_branch(dest_block)
                        .unwrap();
                }
                mir::Termination::Switch {
                    condition,
                    then_bb,
                    else_bb,
                } => {
                    let cond_val = match condition.local {
                        mir::PlaceKind::Param(param_idx) => {
                            if let Some(projection) = condition.projection {
                                match projection {
                                    mir::Projection::Field { idx, ty, name: _ } => {
                                        self.load_param_struct_gep(param_idx, idx, ty)
                                    }
                                }
                            } else {
                                self.load_param(param_idx)
                            }
                        }
                        mir::PlaceKind::Local(local_idx) => {
                            if let Some(projection) = condition.projection {
                                match projection {
                                    mir::Projection::Field { idx, ty, name: _ } => {
                                        self.load_local_struct_gep(local_idx, idx, ty)
                                    }
                                }
                            } else {
                                self.load_local(local_idx)
                            }
                        }
                    };
                    let cond_iv = cond_val.into_int_value();

                    let then_bb = self.basic_blocks[then_bb];
                    let else_bb = self.basic_blocks[else_bb];
                    self.codegen
                        .builder
                        .build_switch(
                            cond_iv,
                            else_bb,
                            &[(self.codegen.bool_type().const_int(1, false), then_bb)],
                        )
                        .unwrap();
                }
                mir::Termination::Call {
                    function,
                    args,
                    destination,
                    target,
                } => {
                    let callee_fn = self.codegen.defined_functions[function];
                    let arg_vals: Vec<_> = args
                        .iter()
                        .map(|arg| self.gen_operand(arg).into())
                        .collect();

                    let call_site = self
                        .codegen
                        .builder
                        .build_call(callee_fn, &arg_vals, "call")
                        .unwrap();

                    let ret_val = call_site.try_as_basic_value().left().unwrap();

                    match destination.local {
                        mir::PlaceKind::Param(_) => unreachable!(),
                        mir::PlaceKind::Local(local_idx) => {
                            if let Some(projection) = destination.projection {
                                match projection {
                                    mir::Projection::Field { idx, .. } => {
                                        self.store_local_struct_gep(local_idx, idx, ret_val);
                                    }
                                }
                            } else {
                                self.store_local(local_idx, ret_val);
                            }
                        }
                    }

                    let target_bb = self.basic_blocks[target];
                    self.codegen
                        .builder
                        .build_unconditional_branch(target_bb)
                        .unwrap();
                }
            }
        }
    }

    fn gen_operand(&self, operand: &mir::Operand) -> BasicValueEnum<'ctx> {
        match operand {
            mir::Operand::Place(place) => match place.local {
                mir::PlaceKind::Param(param_idx) => {
                    if let Some(projection) = place.projection {
                        match projection {
                            mir::Projection::Field { idx, ty, name: _ } => {
                                self.load_param_struct_gep(param_idx, idx, ty)
                            }
                        }
                    } else {
                        self.load_param(param_idx)
                    }
                }
                mir::PlaceKind::Local(local_idx) => {
                    if let Some(projection) = place.projection {
                        match projection {
                            mir::Projection::Field { idx, ty, name: _ } => {
                                self.load_local_struct_gep(local_idx, idx, ty)
                            }
                        }
                    } else {
                        self.load_local(local_idx)
                    }
                }
            },
            mir::Operand::Constant(constant) => match constant {
                mir::Constant::Integer(integer) => self
                    .codegen
                    .integer_type()
                    .const_int(*integer, false)
                    .into(),
                mir::Constant::Boolean(boolean) => self
                    .codegen
                    .bool_type()
                    .const_int(if *boolean { 1 } else { 0 }, false)
                    .into(),
                mir::Constant::String(string) => self.codegen.build_string_value(string).into(),
                mir::Constant::Unit => self.codegen.unit_type().const_zero().into(),
                mir::Constant::StructUnit(_) => self.codegen.unit_type().const_zero().into(),
            },
        }
    }

    fn gen_int_binary_op(
        &self,
        op: &mir::BinaryOp,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        match op {
            mir::BinaryOp::Add => self
                .codegen
                .builder
                .build_int_add(lhs, rhs, "add_number")
                .unwrap(),
            mir::BinaryOp::Sub => self
                .codegen
                .builder
                .build_int_sub(lhs, rhs, "sub_number")
                .unwrap(),
            mir::BinaryOp::Mul => self
                .codegen
                .builder
                .build_int_mul(lhs, rhs, "mul_number")
                .unwrap(),
            mir::BinaryOp::Div => self
                .codegen
                .builder
                .build_int_signed_div(lhs, rhs, "div_number")
                .unwrap(),
            mir::BinaryOp::Equal => self
                .codegen
                .builder
                .build_int_compare(inkwell::IntPredicate::EQ, lhs, rhs, "equal_number")
                .unwrap(),
            mir::BinaryOp::NotEq => self
                .codegen
                .builder
                .build_int_compare(inkwell::IntPredicate::NE, lhs, rhs, "not_equal_number")
                .unwrap(),
            mir::BinaryOp::GreaterThan => self
                .codegen
                .builder
                .build_int_compare(inkwell::IntPredicate::SGT, lhs, rhs, "gt_number")
                .unwrap(),
            mir::BinaryOp::LessThan => self
                .codegen
                .builder
                .build_int_compare(inkwell::IntPredicate::SLT, lhs, rhs, "lt_number")
                .unwrap(),
            mir::BinaryOp::GtEq => self
                .codegen
                .builder
                .build_int_compare(inkwell::IntPredicate::SGE, lhs, rhs, "gteq_number")
                .unwrap(),
            mir::BinaryOp::LtEq => self
                .codegen
                .builder
                .build_int_compare(inkwell::IntPredicate::SLE, lhs, rhs, "lteq_number")
                .unwrap(),
        }
    }

    fn gen_int_unary_op(&self, op: &mir::UnaryOp, expr: IntValue<'ctx>) -> IntValue<'ctx> {
        match op {
            mir::UnaryOp::Neg => self
                .codegen
                .builder
                .build_int_neg(expr, "neg_number")
                .unwrap(),
            mir::UnaryOp::Not => self.codegen.builder.build_not(expr, "not_bool").unwrap(),
        }
    }
}
