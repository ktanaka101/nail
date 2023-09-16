use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    types::BasicTypeEnum,
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
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
                hir_ty::Monotype::Never => todo!(),
                hir_ty::Monotype::Function(_) => todo!(),
                hir_ty::Monotype::Variable(_) => unreachable!(""),
                hir_ty::Monotype::Unknown => unreachable!(),
            };

            let local_ptr = self
                .codegen
                .builder
                .build_alloca(ty, &local.idx.to_string());
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
                self.codegen.builder.build_unconditional_branch(appneded_bb);
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
        self.codegen.builder.build_store(ptr.1, value);
    }

    fn load_local(&self, local_idx: mir::LocalIdx) -> BasicValueEnum<'ctx> {
        let local = self.locals[&local_idx];
        self.codegen.builder.build_load(local.0, local.1, "load")
    }

    fn load_param(&self, param_idx: mir::ParamIdx) -> BasicValueEnum<'ctx> {
        let param = &self.body.params[param_idx];
        self.function.get_nth_param(param.pos).unwrap()
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
                                    .into(),
                                mir::BinaryOp::Sub => self
                                    .codegen
                                    .builder
                                    .build_int_sub(lhs, rhs, "sub_number")
                                    .into(),
                                mir::BinaryOp::Mul => self
                                    .codegen
                                    .builder
                                    .build_int_mul(lhs, rhs, "mul_number")
                                    .into(),
                                mir::BinaryOp::Div => self
                                    .codegen
                                    .builder
                                    .build_int_signed_div(lhs, rhs, "div_number")
                                    .into(),
                                mir::BinaryOp::Equal => self
                                    .codegen
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::EQ,
                                        lhs,
                                        rhs,
                                        "compare_number",
                                    )
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
                                    .into(),
                                mir::UnaryOp::Not => {
                                    self.codegen.builder.build_not(expr, "not_bool").into()
                                }
                            }
                        }
                    };

                    match place {
                        mir::Place::Param(_param) => unreachable!(),
                        mir::Place::Local(local) => self.store_local(*local, val),
                    }
                }
            }
        }

        if let Some(termination) = &bb.termination {
            match termination {
                mir::Termination::Return(ret) => {
                    let ret_local = self.locals[ret];
                    let ret_local =
                        self.codegen
                            .builder
                            .build_load(ret_local.0, ret_local.1, "load");
                    self.codegen.builder.build_return(Some(&ret_local));
                }
                mir::Termination::Goto(goto) => {
                    let dest_block = self.basic_blocks[goto];
                    self.codegen.builder.build_unconditional_branch(dest_block);
                }
                mir::Termination::Switch {
                    condition,
                    then_bb,
                    else_bb,
                } => {
                    let cond = match condition {
                        mir::Place::Param(param) => self.load_param(*param),
                        mir::Place::Local(local) => self.load_local(*local),
                    };
                    let cond = cond.into_int_value();
                    let then_bb = self.basic_blocks[then_bb];
                    let else_bb = self.basic_blocks[else_bb];
                    self.codegen.builder.build_switch(
                        cond,
                        else_bb,
                        &[(self.codegen.bool_type().const_int(1, false), then_bb)],
                    );
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

                    let call_site_value =
                        self.codegen
                            .builder
                            .build_call(callee_function, &args, "call");

                    match destination {
                        mir::Place::Param(_) => unreachable!(),
                        mir::Place::Local(local) => self.store_local(
                            *local,
                            call_site_value.try_as_basic_value().left().unwrap(),
                        ),
                    };

                    let target_bb = self.basic_blocks[target];
                    self.codegen.builder.build_unconditional_branch(target_bb);
                }
            }
        }
    }

    fn gen_operand(&self, operand: &mir::Operand) -> BasicValueEnum<'ctx> {
        match operand {
            mir::Operand::Place(place) => match place {
                mir::Place::Param(param) => self.load_param(*param),
                mir::Place::Local(local) => self.load_local(*local),
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
            },
        }
    }
}
