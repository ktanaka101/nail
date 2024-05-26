use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};

use crate::Codegen;

pub(crate) struct BodyCodegen<'a, 'ctx> {
    codegen: &'a Codegen<'a, 'ctx>,

    function_id: mir::FunctionId,
    function: FunctionValue<'ctx>,
    body: &'a mir::Body,

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

        // register alloc locals.
        for (idx, local) in self.body.locals.iter() {
            let ty: BasicTypeEnum = match local.ty {
                hir_ty::Monotype::Unit => self.codegen.unit_type().into(),
                hir_ty::Monotype::Integer => self.codegen.integer_type().into(),
                hir_ty::Monotype::String => self.codegen.string_type().into(),
                hir_ty::Monotype::Bool => self.codegen.bool_type().into(),
                hir_ty::Monotype::Char => todo!(),
                hir_ty::Monotype::Struct(struct_) => {
                    let struct_ty = self.codegen.declaration_structs.get(&struct_).unwrap();
                    struct_ty.as_basic_type_enum()
                }
                // Never用にUnitを確保した方がいいかも？
                hir_ty::Monotype::Never => continue,
                hir_ty::Monotype::Function(_) => todo!(),
                hir_ty::Monotype::Variable(_) => unreachable!(""),
                hir_ty::Monotype::Unknown => unreachable!(),
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
            let appneded_bb = self
                .codegen
                .context
                .append_basic_block(self.function, &bb.name());
            self.basic_blocks.insert(bb_idx, appneded_bb);

            if is_entry {
                is_entry = false;
                self.codegen
                    .builder
                    .build_unconditional_branch(appneded_bb)
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
        let ptr = self.locals[&local_idx];
        self.codegen.builder.build_store(ptr.1, value).unwrap();
    }

    fn store_local_struct_gep(
        &self,
        local_idx: mir::LocalIdx,
        idx: u32,
        value: BasicValueEnum<'ctx>,
    ) {
        let ptr = self.locals[&local_idx];

        let ptr = self
            .codegen
            .builder
            .build_struct_gep(ptr.0.into_struct_type(), ptr.1, idx, "field")
            .unwrap();

        self.codegen.builder.build_store(ptr, value).unwrap();
    }

    fn load_local(&self, local_idx: mir::LocalIdx) -> BasicValueEnum<'ctx> {
        let local = self.locals[&local_idx];
        match local.0 {
            BasicTypeEnum::StructType(_) => {
                // 構造体型をPointerにしてからloadする
                let ptr = self
                    .codegen
                    .builder
                    .build_load(
                        self.codegen.context.ptr_type(AddressSpace::default()),
                        local.1,
                        "load",
                    )
                    .unwrap()
                    .into_pointer_value();
                self.codegen
                    .builder
                    .build_load(local.0, ptr, "load")
                    .unwrap()
            }

            _ => self
                .codegen
                .builder
                .build_load(local.0, local.1, "load")
                .unwrap(),
        }
    }

    fn load_local_struct_gep(&self, local_idx: mir::LocalIdx, idx: u32) -> BasicValueEnum<'ctx> {
        let local = self.locals[&local_idx];

        let ptr = self
            .codegen
            .builder
            .build_struct_gep(local.0, local.1, idx, "gep_field")
            .unwrap();

        self.codegen
            .builder
            .build_load(ptr.get_type(), ptr, "load_field")
            .unwrap()
    }

    fn load_param(&self, param_idx: mir::ParamIdx) -> BasicValueEnum<'ctx> {
        let param = &self.body.params[param_idx];
        self.function.get_nth_param(param.pos).unwrap()
    }

    fn load_param_struct_gep(&self, param_idx: mir::ParamIdx, idx: u32) -> BasicValueEnum<'ctx> {
        let param = &self.body.params[param_idx];
        let ptr = self
            .codegen
            .builder
            .build_struct_gep(
                self.function
                    .get_nth_param(param.pos)
                    .unwrap()
                    .get_type()
                    .into_struct_type(),
                self.function
                    .get_nth_param(param.pos)
                    .unwrap()
                    .into_pointer_value(),
                idx,
                "field",
            )
            .unwrap();

        self.codegen
            .builder
            .build_load(ptr.get_type(), ptr, "load")
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
                            match op {
                                mir::BinaryOp::Add => self
                                    .codegen
                                    .builder
                                    .build_int_add(lhs, rhs, "add_number")
                                    .unwrap()
                                    .into(),
                                mir::BinaryOp::Sub => self
                                    .codegen
                                    .builder
                                    .build_int_sub(lhs, rhs, "sub_number")
                                    .unwrap()
                                    .into(),
                                mir::BinaryOp::Mul => self
                                    .codegen
                                    .builder
                                    .build_int_mul(lhs, rhs, "mul_number")
                                    .unwrap()
                                    .into(),
                                mir::BinaryOp::Div => self
                                    .codegen
                                    .builder
                                    .build_int_signed_div(lhs, rhs, "div_number")
                                    .unwrap()
                                    .into(),
                                mir::BinaryOp::Equal => self
                                    .codegen
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::EQ,
                                        lhs,
                                        rhs,
                                        "equal_number",
                                    )
                                    .unwrap()
                                    .into(),
                                mir::BinaryOp::NotEq => self
                                    .codegen
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::NE,
                                        lhs,
                                        rhs,
                                        "not_equal_number",
                                    )
                                    .unwrap()
                                    .into(),
                                mir::BinaryOp::GreaterThan => self
                                    .codegen
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::SGT,
                                        lhs,
                                        rhs,
                                        "gt_number",
                                    )
                                    .unwrap()
                                    .into(),
                                mir::BinaryOp::LessThan => self
                                    .codegen
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::SLT,
                                        lhs,
                                        rhs,
                                        "lt_number",
                                    )
                                    .unwrap()
                                    .into(),
                                mir::BinaryOp::GtEq => self
                                    .codegen
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::SGE,
                                        lhs,
                                        rhs,
                                        "gteq_number",
                                    )
                                    .unwrap()
                                    .into(),
                                mir::BinaryOp::LtEq => self
                                    .codegen
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::SLE,
                                        lhs,
                                        rhs,
                                        "lteq_number",
                                    )
                                    .unwrap()
                                    .into(),
                            }
                        }
                        mir::Value::UnaryOp { op, expr } => {
                            let expr = self.gen_operand(expr).into_int_value();
                            match op {
                                mir::UnaryOp::Neg => self
                                    .codegen
                                    .builder
                                    .build_int_neg(expr, "neg_number")
                                    .unwrap()
                                    .into(),
                                mir::UnaryOp::Not => self
                                    .codegen
                                    .builder
                                    .build_not(expr, "not_bool")
                                    .unwrap()
                                    .into(),
                            }
                        }
                        mir::Value::Aggregate { kind, operands } => match kind {
                            mir::AggregateKind::Struct(struct_) => {
                                let mut values = vec![];
                                for operand in operands {
                                    let operand = self.gen_operand(operand);
                                    values.push(operand);
                                }

                                let struct_ty = self.codegen.declaration_structs[struct_];

                                let struct_ptr = self
                                    .codegen
                                    .builder
                                    .build_alloca(struct_ty, "struct_val")
                                    .unwrap();

                                for (i, value) in values.iter().enumerate() {
                                    let field_ptr = self
                                        .codegen
                                        .builder
                                        .build_struct_gep(
                                            struct_ty,
                                            struct_ptr,
                                            i.try_into().unwrap(),
                                            "struct_val_field",
                                        )
                                        .unwrap();
                                    self.codegen
                                        .builder
                                        .build_store(field_ptr, value.as_basic_value_enum())
                                        .unwrap();
                                }

                                struct_ptr.into()
                            }
                        },
                    };

                    match place.local {
                        mir::PlaceKind::Param(_param) => unreachable!(),
                        mir::PlaceKind::Local(local) => {
                            if let Some(projection) = place.projection {
                                match projection {
                                    mir::Projection::Field { idx, .. } => {
                                        self.store_local_struct_gep(local, idx, val);
                                    }
                                }
                            } else {
                                self.store_local(local, val);
                            }
                        }
                    }
                }
            }
        }

        if let Some(termination) = &bb.termination {
            match termination {
                mir::Termination::Return(ret) => {
                    let ret_local = self.locals[ret];
                    let ret_local = self
                        .codegen
                        .builder
                        .build_load(ret_local.0, ret_local.1, "load")
                        .unwrap();
                    self.codegen.builder.build_return(Some(&ret_local)).unwrap();
                }
                mir::Termination::Goto(goto) => {
                    let dest_block = self.basic_blocks[goto];
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
                    let cond = match condition.local {
                        mir::PlaceKind::Param(param) => {
                            if let Some(projection) = condition.projection {
                                match projection {
                                    mir::Projection::Field { idx, .. } => {
                                        self.load_param_struct_gep(param, idx)
                                    }
                                }
                            } else {
                                self.load_param(param)
                            }
                        }
                        mir::PlaceKind::Local(local) => {
                            if let Some(projection) = condition.projection {
                                match projection {
                                    mir::Projection::Field { idx, .. } => {
                                        self.load_local_struct_gep(local, idx)
                                    }
                                }
                            } else {
                                self.load_local(local)
                            }
                        }
                    };
                    let cond = cond.into_int_value();
                    let then_bb = self.basic_blocks[then_bb];
                    let else_bb = self.basic_blocks[else_bb];
                    self.codegen
                        .builder
                        .build_switch(
                            cond,
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
                    let callee_function = self.codegen.defined_functions[function];
                    let args = args
                        .iter()
                        .map(|arg| self.gen_operand(arg).into())
                        .collect::<Vec<BasicMetadataValueEnum<'ctx>>>();

                    let call_site_value = self
                        .codegen
                        .builder
                        .build_call(callee_function, &args, "call")
                        .unwrap();

                    match destination.local {
                        mir::PlaceKind::Param(_) => unreachable!(),
                        mir::PlaceKind::Local(local) => {
                            if let Some(projection) = destination.projection {
                                match projection {
                                    mir::Projection::Field { idx, .. } => {
                                        self.store_local_struct_gep(
                                            local,
                                            idx,
                                            call_site_value.try_as_basic_value().left().unwrap(),
                                        );
                                    }
                                }
                            } else {
                                self.store_local(
                                    local,
                                    call_site_value.try_as_basic_value().left().unwrap(),
                                );
                            }
                        }
                    };

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
                mir::PlaceKind::Param(param) => {
                    if let Some(projection) = place.projection {
                        match projection {
                            mir::Projection::Field { idx, .. } => {
                                self.load_param_struct_gep(param, idx)
                            }
                        }
                    } else {
                        self.load_param(param)
                    }
                }
                mir::PlaceKind::Local(local) => {
                    if let Some(projection) = place.projection {
                        match projection {
                            mir::Projection::Field { idx, .. } => {
                                self.load_local_struct_gep(local, idx)
                            }
                        }
                    } else {
                        self.load_local(local)
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
                mir::Constant::StructUnit(struct_) => {
                    let struct_ty = self.codegen.declaration_structs[struct_];
                    struct_ty.const_zero().into()
                }
            },
        }
    }
}
