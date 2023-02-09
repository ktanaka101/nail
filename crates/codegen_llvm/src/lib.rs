mod builtin_function;

use std::collections::HashMap;

use either::Either;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType, IntType, PointerType, StructType},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};

const FN_ENTRY_BLOCK_NAME: &str = "start";
const INTERNAL_ENTRY_POINT: &str = "__main__";

pub fn codegen<'a, 'ctx>(
    hir_result: &'a hir::LowerResult,
    ty_result: &'a hir_ty::TyLowerResult,
    mir_result: &'a mir::LowerResult,
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    execution_engine: &'a ExecutionEngine<'ctx>,
    should_return_string: bool,
) -> CodegenResult<'ctx> {
    let codegen = Codegen::new(
        hir_result,
        ty_result,
        mir_result,
        context,
        module,
        builder,
        execution_engine,
    );
    codegen.gen(should_return_string)
}

type MainFunc = unsafe extern "C" fn() -> *mut i8;
pub struct CodegenResult<'ctx> {
    #[allow(dead_code)]
    function: JitFunction<'ctx, MainFunc>,
}

struct BodyCodegen<'a, 'ctx> {
    codegen: &'a Codegen<'a, 'ctx>,

    function_id: mir::FunctionId,
    function: FunctionValue<'ctx>,
    body: &'a mir::Body,

    params: HashMap<mir::ParamIdx, PointerValue<'ctx>>,
    locals: HashMap<mir::LocalIdx, (BasicTypeEnum<'ctx>, PointerValue<'ctx>)>,
    basic_blocks: HashMap<mir::BasicBlockIdx, BasicBlock<'ctx>>,
}
impl<'a, 'ctx> BodyCodegen<'a, 'ctx> {
    fn new(
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
            params: HashMap::new(),
            locals: HashMap::new(),
            basic_blocks: HashMap::new(),
        }
    }

    fn gen(mut self) {
        self.codegen.move_to_entry_bb_in_function(self.function_id);

        // register alloc locals.
        for (idx, local) in self.body.locals.iter() {
            let ty: BasicTypeEnum = match local.ty {
                hir_ty::ResolvedType::Unit => self.codegen.unit_type().into(),
                hir_ty::ResolvedType::Integer => self.codegen.integer_type().into(),
                hir_ty::ResolvedType::String => self.codegen.string_type().into(),
                hir_ty::ResolvedType::Bool => self.codegen.bool_type().into(),
                _ => unimplemented!(),
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

            self.gen_bb(bb_idx, bb);
        }
    }

    fn move_to_bb_by_bb_idx(&self, bb_idx: mir::BasicBlockIdx) {
        let target_bb = self.basic_blocks.get(&bb_idx).unwrap();
        self.codegen.builder.position_at_end(*target_bb);
    }

    fn gen_bb(&self, idx: mir::BasicBlockIdx, bb: &mir::BasicBlock) {
        for stmt in &bb.statements {
            match stmt {
                mir::Statement::Assign { place, value } => {
                    let ptr = match place {
                        mir::Place::Param(_) => todo!(),
                        mir::Place::Local(local) => self.locals[local],
                    };
                    let val = match value {
                        mir::Value::Operand(operand) => self.gen_operand(operand),
                        mir::Value::BinaryOp { op, left, right } => todo!(),
                    };

                    self.codegen.builder.build_store(ptr.1, val);
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
                        mir::Place::Param(_) => todo!(),
                        mir::Place::Local(local) => self.locals[local],
                    };
                    let cond = self.codegen.builder.build_load(cond.0, cond.1, "load");
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

                    let destination = match destination {
                        mir::Place::Param(_) => todo!(),
                        mir::Place::Local(local) => self.locals[local],
                    };
                    let call_site_value =
                        self.codegen
                            .builder
                            .build_call(callee_function, &args, "call");
                    self.codegen.builder.build_store(
                        destination.1,
                        call_site_value.try_as_basic_value().left().unwrap(),
                    );

                    let target_bb = self.basic_blocks[target];
                    self.codegen.builder.build_unconditional_branch(target_bb);
                }
            }
        }
    }

    fn gen_operand(&self, operand: &mir::Operand) -> BasicValueEnum<'ctx> {
        match operand {
            mir::Operand::Place(place) => {
                let ptr = match place {
                    mir::Place::Param(_) => todo!(),
                    mir::Place::Local(local) => self.locals[local],
                };
                self.codegen.builder.build_load(ptr.0, ptr.1, "load")
            }
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
                mir::Constant::Unit => self.codegen.unit_type().const_zero().into(),
            },
        }
    }
}

struct Codegen<'a, 'ctx> {
    hir_result: &'a hir::LowerResult,
    ty_result: &'a hir_ty::TyLowerResult,
    mir_result: &'a mir::LowerResult,

    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    execution_engine: &'a ExecutionEngine<'ctx>,

    builtin_functions: HashMap<String, FunctionValue<'ctx>>,

    defined_functions: HashMap<mir::FunctionId, FunctionValue<'ctx>>,
    entry_blocks: HashMap<mir::FunctionId, BasicBlock<'ctx>>,
}

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    fn new(
        hir_result: &'a hir::LowerResult,
        ty_result: &'a hir_ty::TyLowerResult,
        mir_result: &'a mir::LowerResult,
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        execution_engine: &'a ExecutionEngine<'ctx>,
    ) -> Self {
        let mut codegen = Self {
            hir_result,
            ty_result,
            mir_result,
            context,
            module,
            builder,
            execution_engine,
            builtin_functions: HashMap::new(),
            defined_functions: HashMap::new(),
            entry_blocks: HashMap::new(),
        };
        codegen.add_builtin_function();

        codegen
    }

    fn move_to_entry_bb_in_function(&self, function_id: mir::FunctionId) {
        let entry_block = self.entry_blocks.get(&function_id).unwrap();
        self.builder.position_at_end(*entry_block);
    }

    fn gen(mut self, should_return_string: bool) -> CodegenResult<'ctx> {
        if self.mir_result.entry_point().is_none() {
            unimplemented!();
        }

        self.gen_function_signatures();
        self.gen_functions();

        let result = {
            let fn_type = self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .fn_type(&[], false);
            let entry_point = self
                .module
                .add_function(INTERNAL_ENTRY_POINT, fn_type, None);
            let inner_entry_point_block = self
                .context
                .append_basic_block(entry_point, FN_ENTRY_BLOCK_NAME);
            self.builder.position_at_end(inner_entry_point_block);
            self.builder.build_call(
                self.defined_functions[&self.mir_result.function_id_of_entry_point().unwrap()],
                &[],
                "call_entry_point",
            )
        };

        if should_return_string {
            match result.try_as_basic_value() {
                Either::Left(result_value) => {
                    let call_v = self.build_to_string(result_value);
                    let return_v = call_v.try_as_basic_value().left().unwrap();

                    self.builder.build_return(Some(&return_v));
                }
                Either::Right(_) => unreachable!(),
            }
        } else {
            self.builder.build_return(None);
        }

        CodegenResult {
            function: unsafe {
                self.execution_engine
                    .get_function(INTERNAL_ENTRY_POINT)
                    .unwrap()
            },
        }
    }

    fn body_to_params(&self, body: &mir::Body) -> Vec<BasicMetadataTypeEnum<'ctx>> {
        body.params
            .iter()
            .map(|(_, param)| match param.ty {
                hir_ty::ResolvedType::Integer => {
                    BasicMetadataTypeEnum::IntType(self.context.i64_type())
                }
                hir_ty::ResolvedType::String => self
                    .context
                    .i8_type()
                    .vec_type(1)
                    .ptr_type(AddressSpace::default())
                    .into(),
                hir_ty::ResolvedType::Bool => self.context.bool_type().into(),
                _ => unimplemented!(),
            })
            .collect::<Vec<_>>()
    }

    fn lookup_name(&self, name: &hir::Name) -> &str {
        self.hir_result.interner.lookup(name.key())
    }

    fn unit_type(&self) -> StructType<'ctx> {
        self.context.struct_type(&[], false)
    }

    fn integer_type(&self) -> IntType<'ctx> {
        self.context.i64_type()
    }

    fn string_type(&self) -> PointerType<'ctx> {
        self.context.i8_type().ptr_type(AddressSpace::default())
    }

    fn bool_type(&self) -> IntType<'ctx> {
        self.context.bool_type()
    }

    fn build_string_value(&self, string: &str) -> PointerValue<'ctx> {
        self.builder
            .build_global_string_ptr(&format!("{string}\0"), "const_string")
            .as_pointer_value()
    }

    fn gen_function_signatures(&mut self) {
        for (idx, body) in self.mir_result.ref_bodies() {
            let params = self.body_to_params(body);
            let return_type = body.locals[body.return_local].ty;

            let fn_ty: FunctionType<'ctx> = match return_type {
                hir_ty::ResolvedType::Unit => {
                    let ty = self.context.struct_type(&[], false);
                    ty.fn_type(&params, false)
                }
                hir_ty::ResolvedType::Integer => {
                    let ty = self.context.i64_type();
                    ty.fn_type(&params, false)
                }
                hir_ty::ResolvedType::String => {
                    let ty = self.string_type();
                    ty.fn_type(&params, false)
                }
                hir_ty::ResolvedType::Bool => {
                    let ty = self.context.bool_type();
                    ty.fn_type(&params, false)
                }
                _ => unimplemented!(),
            };

            let function_name = self.lookup_name(&body.name);
            let function = self.module.add_function(function_name, fn_ty, None);
            let function_id = self.mir_result.function_id_by_body_idx(idx);
            self.defined_functions.insert(function_id, function);

            let start_block = self
                .context
                .append_basic_block(function, FN_ENTRY_BLOCK_NAME);
            self.builder.position_at_end(start_block);
            self.entry_blocks.insert(function_id, start_block);
        }
    }

    fn gen_functions(&mut self) {
        for (idx, body) in self.mir_result.ref_bodies() {
            let function_id = self.mir_result.function_id_by_body_idx(idx);
            let function_value = self.defined_functions.get(&function_id).unwrap();

            let body_codegen = BodyCodegen::new(self, function_id, *function_value, body);
            body_codegen.gen();
        }
    }

    // fn gen_stmt(&mut self, stmt: &hir::Stmt) {
    //     match stmt {
    //         hir::Stmt::ExprStmt { expr, .. } => {
    //             self.gen_expr(*expr);
    //         }
    //         hir::Stmt::VariableDef { name, value } => {
    //             let right_value = self.gen_expr(*value);
    //             let ptr = self.builder.build_alloca(
    //                 right_value.get_type(),
    //                 &format!("alloca_{}", self.lookup_name(name)),
    //             );
    //             self.builder.build_store(ptr, right_value);
    //         }
    //         hir::Stmt::FunctionDef { .. } => unreachable!(),
    //     }
    // }

    // fn gen_expr(&mut self, expr: hir::ExprIdx) -> BasicValueEnum<'ctx> {
    //     let expr = &self.hir_result.shared_ctx.exprs[expr];
    //     match expr {
    //         hir::Expr::Literal(literal) => match literal {
    //             hir::Literal::Integer(value) => {
    //                 self.context.i64_type().const_int(*value, false).into()
    //             }
    //             hir::Literal::String(string) => self.build_string_value(string).into(),
    //             hir::Literal::Bool(bool) => self
    //                 .context
    //                 .bool_type()
    //                 .const_int(if *bool { 1 } else { 0 }, false)
    //                 .into(),
    //             _ => unimplemented!(),
    //         },
    //         hir::Expr::Binary { op, lhs, rhs } => {
    //             let lhs = self.gen_expr(*lhs).into_int_value();
    //             let rhs = self.gen_expr(*rhs).into_int_value();
    //             match op {
    //                 hir::BinaryOp::Add => self.builder.build_int_add(lhs, rhs, "add_number").into(),
    //                 hir::BinaryOp::Sub => self.builder.build_int_sub(lhs, rhs, "sub_number").into(),
    //                 hir::BinaryOp::Mul => self.builder.build_int_mul(lhs, rhs, "mul_number").into(),
    //                 hir::BinaryOp::Div => self
    //                     .builder
    //                     .build_int_signed_div(lhs, rhs, "div_number")
    //                     .into(),
    //                 hir::BinaryOp::Equal => self
    //                     .builder
    //                     .build_int_compare(inkwell::IntPredicate::EQ, lhs, rhs, "compare_number")
    //                     .const_cast(self.context.bool_type(), false)
    //                     .into(),
    //             }
    //         }
    //         hir::Expr::Unary { op, expr } => {
    //             let expr = self.gen_expr(*expr).into_int_value();
    //             match op {
    //                 hir::UnaryOp::Neg => self.builder.build_int_neg(expr, "neg_number").into(),
    //             }
    //         }
    //         hir::Expr::VariableRef { var } => match var {
    //             hir::Symbol::Local { name, expr } => {
    //                 let (defined_ty, defined_ptr) = &self.defined_variables[expr];
    //                 let name = self.lookup_name(name);
    //                 self.builder
    //                     .build_load(*defined_ty, *defined_ptr, &format!("load_{name}"))
    //             }
    //             hir::Symbol::Param { param, .. } => {
    //                 let function = self.curr_function.unwrap();
    //                 let param = &self.hir_result.db.params[*param];
    //                 function
    //                     .get_nth_param(param.pos.try_into().unwrap())
    //                     .unwrap()
    //             }
    //             _ => unimplemented!(),
    //         },
    //         hir::Expr::Block(block) => {
    //             for stmt in &block.stmts {
    //                 self.gen_stmt(stmt);
    //             }
    //             if let Some(tail) = block.tail {
    //                 self.gen_expr(tail)
    //             } else {
    //                 self.context.const_struct(&[], false).into()
    //             }
    //         }
    //         hir::Expr::Call { callee, args } => match callee {
    //             hir::Symbol::Function { name, function } => {
    //                 let function = self.defined_functions[function];
    //                 let args = args
    //                     .iter()
    //                     .map(|arg| self.gen_expr(*arg).into())
    //                     .collect::<Vec<_>>();
    //                 let call_value = self.builder.build_call(
    //                     function,
    //                     &args,
    //                     &format!("call_{}", self.lookup_name(name)),
    //                 );
    //                 call_value.try_as_basic_value().left().unwrap()
    //             }
    //             _ => unimplemented!(),
    //         },
    //         hir::Expr::If {
    //             condition,
    //             then_branch,
    //             else_branch,
    //         } => {
    //             let condition = self.gen_expr(*condition);
    //             let condition = condition.into_int_value();
    //             if condition.get_type().get_bit_width() != 1 {
    //                 panic!("expected bool because type checked.");
    //             }

    //             let condition = self.builder.build_int_compare(
    //                 inkwell::IntPredicate::EQ,
    //                 condition,
    //                 self.context.bool_type().const_int(1, false),
    //                 "if_condition",
    //             );

    //             let curr_function = self.curr_function.unwrap();

    //             // build branches
    //             let then_bb = self
    //                 .context
    //                 .append_basic_block(curr_function, "then_branch");
    //             let else_bb = self
    //                 .context
    //                 .append_basic_block(curr_function, "else_branch");
    //             let cont_bb = self.context.append_basic_block(curr_function, "ifcont");

    //             self.builder
    //                 .build_conditional_branch(condition, then_bb, else_bb);

    //             // build a then branch
    //             self.builder.position_at_end(then_bb);
    //             let then_val = self.gen_expr(*then_branch);
    //             self.builder.build_unconditional_branch(cont_bb);
    //             let then_bb = self.builder.get_insert_block().unwrap();

    //             // build an else branch
    //             self.builder.position_at_end(else_bb);
    //             let else_val = if let Some(else_branch) = else_branch {
    //                 self.gen_expr(*else_branch)
    //             } else {
    //                 self.context.const_struct(&[], false).into()
    //             };
    //             self.builder.build_unconditional_branch(cont_bb);
    //             let else_bb = self.builder.get_insert_block().unwrap();

    //             // emit merge block
    //             self.builder.position_at_end(cont_bb);
    //             let phi = self
    //                 .builder
    //                 .build_phi(self.context.i64_type(), "if_expr_tmp");
    //             phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
    //             phi.as_basic_value()
    //         }
    //         hir::Expr::Return { value } => {
    //             if let Some(value) = value {
    //                 let return_value = self.gen_expr(*value);
    //                 self.builder.build_return(Some(&return_value));
    //             } else {
    //                 self.builder.build_return(None);
    //             }
    //             self.context.const_struct(&[], false).into()
    //         }
    //         hir::Expr::Missing => unreachable!(),
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use std::ffi::{c_char, CString};

    use ast::AstNode;
    use expect_test::{expect, Expect};
    use inkwell::OptimizationLevel;

    use super::*;

    fn lower(input: &str) -> (hir::LowerResult, hir_ty::TyLowerResult, mir::LowerResult) {
        let parsed = parser::parse(input);
        let ast = ast::SourceFile::cast(parsed.syntax()).unwrap();
        let hir_result = hir::lower(ast);
        let ty_result = hir_ty::lower(&hir_result);
        let mir_result = mir::lower(&hir_result, &ty_result);
        (hir_result, ty_result, mir_result)
    }

    fn check_ir(input: &str, expect: Expect) {
        let (hir_result, ty_result, mir_result) = lower(input);

        let context = Context::create();
        let module = context.create_module("top");
        let builder = context.create_builder();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        codegen(
            &hir_result,
            &ty_result,
            &mir_result,
            &context,
            &module,
            &builder,
            &execution_engine,
            false,
        );
        module.print_to_stderr();

        let ir = module.to_string();
        // Remove environment dependent IR.
        // ex. target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
        let ir = ir
            .lines()
            .filter(|line| !line.starts_with("target datalayout = \""))
            .collect::<Vec<&str>>()
            .join("\n")
            + "\n";

        expect.assert_eq(&ir);
    }

    fn check_result(input: &str, expect: Expect) {
        let (hir_result, ty_result, mir_result) = lower(input);

        let context = Context::create();
        let module = context.create_module("top");
        let builder = context.create_builder();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let result = codegen(
            &hir_result,
            &ty_result,
            &mir_result,
            &context,
            &module,
            &builder,
            &execution_engine,
            true,
        );
        module.print_to_stderr();

        let result_string = {
            let c_string_ptr = unsafe { result.function.call() };
            unsafe { CString::from_raw(c_string_ptr as *mut c_char) }
                .into_string()
                .unwrap()
        };
        expect.assert_eq(&result_string);
    }

    // #[test]
    // fn test_ir_literals() {
    //     check_ir(
    //         r#"
    //         fn main() -> int {
    //             let a = "aaa"
    //             let b = 10
    //             30
    //         }
    //     "#,
    //         expect![[r#"
    //             ; ModuleID = 'top'
    //             source_filename = "top"

    //             @const_string = private unnamed_addr constant [4 x i8] c"aaa\00", align 1

    //             declare ptr @ptr_to_string(i64, ptr, i64)

    //             define i64 @main() {
    //             start:
    //               %alloca_a = alloca ptr, align 8
    //               store ptr @const_string, ptr %alloca_a, align 8
    //               %alloca_b = alloca i64, align 8
    //               store i64 10, ptr %alloca_b, align 8
    //               ret i64 30
    //             }

    //             define ptr @__main__() {
    //             start:
    //               %call_entry_point = call i64 @main()
    //               ret void
    //             }
    //         "#]],
    //     );
    // }

    // #[test]
    // fn test_ir_return_string() {
    //     check_ir(
    //         r#"
    //         fn main() -> string {
    //             let a = "aaa"
    //             a
    //         }
    //     "#,
    //         expect![[r#"
    //             ; ModuleID = 'top'
    //             source_filename = "top"

    //             @const_string = private unnamed_addr constant [4 x i8] c"aaa\00", align 1

    //             declare ptr @ptr_to_string(i64, ptr, i64)

    //             define ptr @main() {
    //             start:
    //               %alloca_a = alloca ptr, align 8
    //               store ptr @const_string, ptr %alloca_a, align 8
    //               %load_a = load ptr, ptr %alloca_a, align 8
    //               ret ptr %load_a
    //             }

    //             define ptr @__main__() {
    //             start:
    //               %call_entry_point = call ptr @main()
    //               ret void
    //             }
    //         "#]],
    //     );
    // }

    #[test]
    fn test_ir_block() {
        check_ir(
            r#"
            fn main() -> int {
                10
                20
                30
            }
        "#,
            expect![[r#"
                ; ModuleID = 'top'
                source_filename = "top"

                declare ptr @ptr_to_string(i64, ptr, i64)

                define i64 @main() {
                start:
                  %"0" = alloca i64, align 8
                  br label %entry

                entry:                                            ; preds = %start
                  store i64 30, ptr %"0", align 8
                  br label %exit

                exit:                                             ; preds = %entry
                  %load = load i64, ptr %"0", align 8
                  ret i64 %load
                }

                define ptr @__main__() {
                start:
                  %call_entry_point = call i64 @main()
                  ret void
                }
            "#]],
        );
    }

    #[test]
    fn test_ir_functions() {
        check_ir(
            r#"
            fn main() -> int {
                10
            }
            fn func() -> int {
                20
            }
        "#,
            expect![[r#"
                ; ModuleID = 'top'
                source_filename = "top"

                declare ptr @ptr_to_string(i64, ptr, i64)

                define i64 @main() {
                start:
                  %"0" = alloca i64, align 8
                  br label %entry

                entry:                                            ; preds = %start
                  store i64 10, ptr %"0", align 8
                  br label %exit

                exit:                                             ; preds = %entry
                  %load = load i64, ptr %"0", align 8
                  ret i64 %load
                }

                define i64 @func() {
                start:
                  %"0" = alloca i64, align 8
                  br label %entry

                entry:                                            ; preds = %start
                  store i64 20, ptr %"0", align 8
                  br label %exit

                exit:                                             ; preds = %entry
                  %load = load i64, ptr %"0", align 8
                  ret i64 %load
                }

                define ptr @__main__() {
                start:
                  %call_entry_point = call i64 @main()
                  ret void
                }
            "#]],
        );
    }

    #[test]
    fn test_ir_defined_variable() {
        check_ir(
            r#"
            fn main() -> int {
                let x = 10
                x
            }
        "#,
            expect![[r#"
                ; ModuleID = 'top'
                source_filename = "top"

                declare ptr @ptr_to_string(i64, ptr, i64)

                define i64 @main() {
                start:
                  %"0" = alloca i64, align 8
                  %"1" = alloca i64, align 8
                  br label %entry

                entry:                                            ; preds = %start
                  store i64 10, ptr %"1", align 8
                  %load = load i64, ptr %"1", align 8
                  store i64 %load, ptr %"0", align 8
                  br label %exit

                exit:                                             ; preds = %entry
                  %load1 = load i64, ptr %"0", align 8
                  ret i64 %load1
                }

                define ptr @__main__() {
                start:
                  %call_entry_point = call i64 @main()
                  ret void
                }
            "#]],
        );
    }

    #[test]
    fn test_return_integer_of_main() {
        check_result(
            r#"
            fn main() -> int {
                100
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 100
                }
            "#]],
        );
    }

    // #[test]
    // fn test_return_string_of_main() {
    //     check_result(
    //         r#"
    //         fn main() -> string {
    //             "aaa"
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "String",
    //               "value": "aaa"
    //             }
    //         "#]],
    //     );
    // }

    #[test]
    fn test_return_bool_of_main() {
        check_result(
            r#"
            fn main() -> bool {
                true
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Boolean",
                  "value": true
                }
            "#]],
        );
    }

    #[test]
    fn test_return_unit_of_main() {
        check_result(
            r#"
            fn main() {
                let a = 10
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Unit",
                  "value": null
                }
            "#]],
        );
    }

    #[test]
    fn test_let_binding() {
        check_result(
            r#"
            fn main() -> int {
                let x = 10
                x
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 10
                }
            "#]],
        );

        // check_result(
        //     r#"
        //     fn main() -> string {
        //         let x = "aaa"
        //         x
        //     }
        // "#,
        //     expect![[r#"
        //         {
        //           "nail_type": "String",
        //           "value": "aaa"
        //         }
        //     "#]],
        // );
    }

    // #[test]
    // fn test_multiple_ref() {
    //     check_result(
    //         r#"
    //         fn main() -> string {
    //             let x = "aaa"
    //             let y = x
    //             let z = y
    //             z
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "String",
    //               "value": "aaa"
    //             }
    //         "#]],
    //     );
    // }

    // #[test]
    // fn test_block() {
    //     check_result(
    //         r#"
    //         fn main() {
    //             {
    //             }
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Unit",
    //               "value": null
    //             }
    //         "#]],
    //     );
    // }

    // #[test]
    // fn test_block_binding() {
    //     check_result(
    //         r#"
    //         fn main() {
    //             let a = {
    //             }
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Unit",
    //               "value": null
    //             }
    //         "#]],
    //     );

    //     check_result(
    //         r#"
    //         fn main() -> int {
    //             let a = {
    //                 10
    //             }
    //             a
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 10
    //             }
    //         "#]],
    //     );
    // }

    #[test]
    fn test_call() {
        check_result(
            r#"
            fn test() -> int {
                10
            }

            fn main() -> int {
                let a = test()
                a
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 10
                }
            "#]],
        );
    }

    // #[test]
    // fn test_call_with_params() {
    //     check_result(
    //         r#"
    //         fn test(x: int) -> int {
    //             x
    //         }

    //         fn main() -> int {
    //             let a = test(10)
    //             a
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 10
    //             }
    //         "#]],
    //     );

    //     check_result(
    //         r#"
    //         fn test(x: int, y: int) -> int {
    //             y
    //         }

    //         fn main() -> int {
    //             let a = test(10, 20)
    //             a
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 20
    //             }
    //         "#]],
    //     );
    // }

    // #[test]
    // fn test_call_with_param_types() {
    //     check_result(
    //         r#"
    //         fn test(x: int) -> int {
    //             x
    //         }

    //         fn main() -> int {
    //             let a = test(10)
    //             a
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 10
    //             }
    //         "#]],
    //     );

    //     check_result(
    //         r#"
    //         fn test(x: string) -> string {
    //             x
    //         }

    //         fn main() -> string {
    //             let a = test("aaa")
    //             a
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "String",
    //               "value": "aaa"
    //             }
    //         "#]],
    //     );

    //     check_result(
    //         r#"
    //         fn test(x: bool) -> bool {
    //             x
    //         }

    //         fn main() -> bool {
    //             let a = test(true)
    //             a
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Boolean",
    //               "value": true
    //             }
    //         "#]],
    //     );
    // }

    // #[test]
    // fn test_gen_signature_preorder() {
    //     check_result(
    //         r#"
    //         fn main() -> bool {
    //             let a = test1(true)
    //             a
    //         }

    //         fn test3(x: bool) -> bool {
    //             x
    //         }

    //         fn test1(x: bool) -> bool {
    //             test2(x)
    //         }

    //         fn test2(x: bool) -> bool {
    //             test3(x)
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Boolean",
    //               "value": true
    //             }
    //         "#]],
    //     );
    // }

    #[test]
    fn test_gen_if_expr() {
        check_result(
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
                {
                  "nail_type": "Int",
                  "value": 10
                }
            "#]],
        );

        check_result(
            r#"
            fn main() -> int {
                if false {
                    10
                } else {
                    20
                }
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 20
                }
            "#]],
        );

        check_result(
            r#"
            fn main() -> int {
                let a = true
                let b = 10
                let c = 20
                if a {
                    b
                } else {
                    c
                }
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 10
                }
            "#]],
        );
    }

    // #[test]
    // fn test_add_number() {
    //     check_result(
    //         r#"
    //         fn main() -> int {
    //             let a = 10
    //             let b = 20
    //             let c = {
    //                 a + 30
    //             }
    //             b + c + 40
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 100
    //             }
    //         "#]],
    //     );
    // }

    // #[test]
    // fn test_sub_number() {
    //     check_result(
    //         r#"
    //         fn main() -> int {
    //             let a = 10
    //             let b = 20
    //             a - b - 30
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": -40
    //             }
    //         "#]],
    //     );
    // }

    // #[test]
    // fn test_mul_number() {
    //     check_result(
    //         r#"
    //         fn main() -> int {
    //             let a = 10
    //             let b = 20
    //             a * b * 30
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 6000
    //             }
    //         "#]],
    //     );
    // }

    // #[test]
    // fn test_div_number() {
    //     check_result(
    //         r#"
    //         fn main() -> int {
    //             let a = 10
    //             let b = 20
    //             2000 / a / b
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 10
    //             }
    //         "#]],
    //     );
    // }

    // #[test]
    // fn test_div_number_truncation() {
    //     check_result(
    //         r#"
    //         fn main() -> int {
    //             let a = 20
    //             let b = 3
    //             a / b
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 6
    //             }
    //         "#]],
    //     );
    // }

    // #[test]
    // fn test_negative_number() {
    //     check_result(
    //         r#"
    //         fn main() -> int {
    //             let a = -10
    //             let b = 20
    //             let c = -30
    //             let d = -{
    //                 40
    //             }
    //             a + (-b) + (-c) - d
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 40
    //             }
    //         "#]],
    //     );
    // }

    // #[test]
    // fn test_equal_number() {
    //     check_result(
    //         r#"
    //         fn main() -> bool {
    //             let a = 10
    //             let b = 10
    //             a == b
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Boolean",
    //               "value": true
    //             }
    //         "#]],
    //     );

    //     check_result(
    //         r#"
    //         fn main() -> bool {
    //             let a = 10
    //             let b = 20
    //             a == b
    //         }
    //     "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Boolean",
    //               "value": false
    //             }
    //         "#]],
    //     );
    // }

    // #[test]
    // fn test_return() {
    //     check_result(
    //         r#"
    //             fn main() -> bool {
    //                 return true
    //             }
    //         "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Boolean",
    //               "value": true
    //             }
    //         "#]],
    //     );

    //     check_result(
    //         r#"
    //             fn main() -> int {
    //                 return 10
    //             }
    //         "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 10
    //             }
    //         "#]],
    //     );

    //     check_result(
    //         r#"
    //             fn main() -> int {
    //                 if false {
    //                     return 10
    //                 } else {
    //                     return 20
    //                 }
    //             }
    //         "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 20
    //             }
    //         "#]],
    //     );

    //     check_result(
    //         r#"
    //             fn main() -> int {
    //                 if true {
    //                     10
    //                 } else {
    //                     return 20
    //                 }
    //             }
    //         "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 10
    //             }
    //         "#]],
    //     );

    //     check_result(
    //         r#"
    //             fn main() -> int {
    //                 if false {
    //                     return 10
    //                 } else {
    //                     20
    //                 }
    //             }
    //         "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 20
    //             }
    //         "#]],
    //     );
    //
    //     check_result(
    //         r#"
    //             fn main() -> int {
    //                 if true {
    //                     return 10
    //                 } else {
    //                     20
    //                 }
    //             }
    //         "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 10
    //             }
    //         "#]],
    //     );

    //     check_result(
    //         r#"
    //             fn main() -> int {
    //                 if false {
    //                     10
    //                 } else {
    //                     return 20
    //                 }
    //             }
    //         "#,
    //         expect![[r#"
    //             {
    //               "nail_type": "Int",
    //               "value": 20
    //             }
    //         "#]],
    //     );
    // }
}
