mod builtin_function;

use std::collections::HashMap;

use either::Either;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType, PointerType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};

const FN_ENTRY_BLOCK_NAME: &str = "start";
const INTERNAL_ENTRY_POINT: &str = "__main__";

pub fn codegen<'a, 'ctx>(
    hir_result: &'a hir::LowerResult,
    ty_result: &'a hir_ty::TyLowerResult,
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    execution_engine: &'a ExecutionEngine<'ctx>,
    should_return_string: bool,
) -> CodegenResult<'ctx> {
    let codegen = Codegen::new(
        hir_result,
        ty_result,
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

struct Codegen<'a, 'ctx> {
    hir_result: &'a hir::LowerResult,
    #[allow(dead_code)]
    ty_result: &'a hir_ty::TyLowerResult,

    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    execution_engine: &'a ExecutionEngine<'ctx>,

    builtin_functions: HashMap<String, FunctionValue<'ctx>>,

    defined_functions: HashMap<hir::FunctionIdx, FunctionValue<'ctx>>,
    entry_blocks: HashMap<hir::FunctionIdx, BasicBlock<'ctx>>,

    defined_variables: HashMap<hir::ExprIdx, (BasicTypeEnum<'ctx>, PointerValue<'ctx>)>,

    curr_function: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    fn new(
        hir_result: &'a hir::LowerResult,
        ty_result: &'a hir_ty::TyLowerResult,
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        execution_engine: &'a ExecutionEngine<'ctx>,
    ) -> Self {
        let mut codegen = Self {
            hir_result,
            ty_result,
            context,
            module,
            builder,
            execution_engine,
            builtin_functions: HashMap::new(),
            defined_functions: HashMap::new(),
            defined_variables: HashMap::new(),
            entry_blocks: HashMap::new(),
            curr_function: None,
        };
        codegen.add_builtin_function();

        codegen
    }

    fn set_function(&mut self, function: FunctionValue<'ctx>) {
        self.curr_function = Some(function);
    }

    fn gen(mut self, should_return_string: bool) -> CodegenResult<'ctx> {
        if self.hir_result.entry_point.is_none() {
            unimplemented!();
        }

        self.gen_function_signatures();
        self.gen_functions();

        let fn_type = self
            .context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .fn_type(&[], false);
        let main_fn = self
            .module
            .add_function(INTERNAL_ENTRY_POINT, fn_type, None);
        self.set_function(main_fn);
        let inner_entry_point_block = self
            .context
            .append_basic_block(main_fn, FN_ENTRY_BLOCK_NAME);
        self.builder.position_at_end(inner_entry_point_block);

        let entry_point = self
            .defined_functions
            .get(&self.hir_result.entry_point.unwrap())
            .unwrap();
        let result = self
            .builder
            .build_call(*entry_point, &[], "call_entry_point");

        if should_return_string {
            match result.try_as_basic_value() {
                Either::Left(result_value) => {
                    let call_v = self.build_to_string(result_value);
                    let return_v = call_v.try_as_basic_value().left().unwrap();

                    self.builder.build_return(Some(&return_v));
                }
                Either::Right(_) => {
                    let call_v = self.build_call_unit_string();
                    let return_v = call_v.try_as_basic_value().left().unwrap();

                    self.builder.build_return(Some(&return_v));
                }
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

    fn signature_to_params(
        &self,
        signature: &hir_ty::Signature,
    ) -> Vec<BasicMetadataTypeEnum<'ctx>> {
        signature
            .params
            .iter()
            .map(|param| match param {
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

    fn string_type(&self) -> PointerType<'ctx> {
        self.context.i8_type().ptr_type(AddressSpace::default())
    }

    fn build_string_value(&self, string: &str) -> PointerValue<'ctx> {
        self.builder
            .build_global_string_ptr(&format!("{string}\0"), "const_string")
            .as_pointer_value()
    }

    fn gen_function_signatures(&mut self) {
        for (idx, function) in self.hir_result.db.functions.iter() {
            let signature = self.ty_result.signature_by_function(&idx);
            let params = self.signature_to_params(signature);

            let fn_ty: FunctionType<'ctx> = match signature.return_type {
                hir_ty::ResolvedType::Unit => {
                    let ty = self.context.void_type();
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

            let function_name = self.lookup_name(&function.name.unwrap());
            let function = self.module.add_function(function_name, fn_ty, None);
            self.defined_functions.insert(idx, function);

            let start_block = self
                .context
                .append_basic_block(function, FN_ENTRY_BLOCK_NAME);
            self.builder.position_at_end(start_block);
            self.entry_blocks.insert(idx, start_block);
        }
    }

    fn gen_functions(&mut self) {
        for (idx, _function) in self.hir_result.db.functions.iter() {
            let function_value = self.defined_functions.get(&idx).unwrap();
            self.set_function(*function_value);

            let entry_block = self.entry_blocks.get(&idx).unwrap();
            self.builder.position_at_end(*entry_block);

            let body_block = self.hir_result.function_body_by_function(&idx).unwrap();
            match body_block {
                hir::Expr::Block(block) => self.gen_body(block),
                _ => unreachable!(),
            }
        }
    }

    fn gen_body(&mut self, block: &hir::Block) {
        for stmt in &block.stmts {
            self.gen_stmt(stmt);
        }
        if let Some(tail) = block.tail.map(|tail| self.gen_expr(tail)) {
            self.builder.build_return(Some(&tail));
        } else {
            self.builder.build_return(None);
        }
    }

    fn gen_stmt(&mut self, stmt: &hir::Stmt) {
        match stmt {
            hir::Stmt::Expr(expr) => {
                self.gen_expr(*expr);
            }
            hir::Stmt::VariableDef { name, value } => {
                let right_value = self.gen_expr(*value);
                let ptr = self.builder.build_alloca(
                    right_value.get_type(),
                    &format!("alloca_{}", self.lookup_name(name)),
                );
                self.builder.build_store(ptr, right_value);
                self.defined_variables
                    .insert(*value, (right_value.get_type(), ptr));
            }
            hir::Stmt::FunctionDef { .. } => unreachable!(),
        }
    }

    fn gen_expr(&mut self, expr: hir::ExprIdx) -> BasicValueEnum<'ctx> {
        let expr = &self.hir_result.shared_ctx.exprs[expr];
        match expr {
            hir::Expr::Literal(literal) => match literal {
                hir::Literal::Integer(value) => {
                    self.context.i64_type().const_int(*value, false).into()
                }
                hir::Literal::String(string) => self.build_string_value(string).into(),
                hir::Literal::Bool(bool) => self
                    .context
                    .bool_type()
                    .const_int(if *bool { 1 } else { 0 }, false)
                    .into(),
                _ => unimplemented!(),
            },
            hir::Expr::Binary { op, lhs, rhs } => {
                let lhs = self.gen_expr(*lhs).into_int_value();
                let rhs = self.gen_expr(*rhs).into_int_value();
                match op {
                    hir::BinaryOp::Add => self.builder.build_int_add(lhs, rhs, "add_number").into(),
                    hir::BinaryOp::Sub => self.builder.build_int_sub(lhs, rhs, "sub_number").into(),
                    hir::BinaryOp::Mul => self.builder.build_int_mul(lhs, rhs, "mul_number").into(),
                    hir::BinaryOp::Div => self
                        .builder
                        .build_int_signed_div(lhs, rhs, "div_number")
                        .into(),
                    hir::BinaryOp::Equal => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, lhs, rhs, "compare_number")
                        .const_cast(self.context.bool_type(), false)
                        .into(),
                }
            }
            hir::Expr::Unary { op, expr } => {
                let expr = self.gen_expr(*expr).into_int_value();
                match op {
                    hir::UnaryOp::Neg => self.builder.build_int_neg(expr, "neg_number").into(),
                }
            }
            hir::Expr::VariableRef { var } => match var {
                hir::Symbol::Local { name, expr } => {
                    let (defined_ty, defined_ptr) = &self.defined_variables[expr];
                    let name = self.lookup_name(name);
                    self.builder
                        .build_load(*defined_ty, *defined_ptr, &format!("load_{name}"))
                }
                hir::Symbol::Param { param, .. } => {
                    let function = self.curr_function.unwrap();
                    let param = &self.hir_result.db.params[*param];
                    function
                        .get_nth_param(param.pos.try_into().unwrap())
                        .unwrap()
                }
                _ => unimplemented!(),
            },
            hir::Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.gen_stmt(stmt);
                }
                if let Some(tail) = block.tail {
                    self.gen_expr(tail)
                } else {
                    self.context.const_struct(&[], false).into()
                }
            }
            hir::Expr::Call { callee, args } => match callee {
                hir::Symbol::Function { name, function } => {
                    let function = self.defined_functions[function];
                    let args = args
                        .iter()
                        .map(|arg| self.gen_expr(*arg).into())
                        .collect::<Vec<_>>();
                    let call_value = self.builder.build_call(
                        function,
                        &args,
                        &format!("call_{}", self.lookup_name(name)),
                    );
                    call_value.try_as_basic_value().left().unwrap()
                }
                _ => unimplemented!(),
            },
            hir::Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.gen_expr(*condition);
                let condition = condition.into_int_value();
                if condition.get_type().get_bit_width() != 1 {
                    panic!("expected bool because type checked.");
                }

                let condition = self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    condition,
                    self.context.bool_type().const_int(1, false),
                    "if_condition",
                );

                let curr_function = self.curr_function.unwrap();

                // build branches
                let then_bb = self
                    .context
                    .append_basic_block(curr_function, "then_branch");
                let else_bb = self
                    .context
                    .append_basic_block(curr_function, "else_branch");
                let cont_bb = self.context.append_basic_block(curr_function, "ifcont");

                self.builder
                    .build_conditional_branch(condition, then_bb, else_bb);

                // build a then branch
                self.builder.position_at_end(then_bb);
                let then_val = self.gen_expr(*then_branch);
                self.builder.build_unconditional_branch(cont_bb);
                let then_bb = self.builder.get_insert_block().unwrap();

                // build an else branch
                self.builder.position_at_end(else_bb);
                let else_val = if let Some(else_branch) = else_branch {
                    self.gen_expr(*else_branch)
                } else {
                    self.context.const_struct(&[], false).into()
                };
                self.builder.build_unconditional_branch(cont_bb);
                let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(cont_bb);
                let phi = self
                    .builder
                    .build_phi(self.context.i64_type(), "if_expr_tmp");
                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
                phi.as_basic_value()
            }
            hir::Expr::Return { value } => todo!(),
            hir::Expr::Missing => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ffi::{c_char, CString};

    use ast::AstNode;
    use expect_test::{expect, Expect};
    use inkwell::OptimizationLevel;

    use super::*;

    fn lower(input: &str) -> (hir::LowerResult, hir_ty::TyLowerResult) {
        let parsed = parser::parse(input);
        let ast = ast::SourceFile::cast(parsed.syntax()).unwrap();
        let hir_result = hir::lower(ast);
        let ty_result = hir_ty::lower(&hir_result);
        (hir_result, ty_result)
    }

    fn check_ir(input: &str, expect: Expect) {
        let (hir_result, ty_result) = lower(input);

        let context = Context::create();
        let module = context.create_module("top");
        let builder = context.create_builder();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        codegen(
            &hir_result,
            &ty_result,
            &context,
            &module,
            &builder,
            &execution_engine,
            false,
        );

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
        let (hir_result, ty_result) = lower(input);

        let context = Context::create();
        let module = context.create_module("top");
        let builder = context.create_builder();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let result = codegen(
            &hir_result,
            &ty_result,
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

    #[test]
    fn test_ir_literals() {
        check_ir(
            r#"
            fn main() -> int {
                let a = "aaa"
                let b = 10
                30
            }
        "#,
            expect![[r#"
                ; ModuleID = 'top'
                source_filename = "top"

                @const_string = private unnamed_addr constant [4 x i8] c"aaa\00", align 1

                declare ptr @ptr_to_string(i64, ptr, i64)

                define i64 @main() {
                start:
                  %alloca_a = alloca ptr, align 8
                  store ptr @const_string, ptr %alloca_a, align 8
                  %alloca_b = alloca i64, align 8
                  store i64 10, ptr %alloca_b, align 8
                  ret i64 30
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
    fn test_ir_return_string() {
        check_ir(
            r#"
            fn main() -> string {
                let a = "aaa"
                a
            }
        "#,
            expect![[r#"
                ; ModuleID = 'top'
                source_filename = "top"

                @const_string = private unnamed_addr constant [4 x i8] c"aaa\00", align 1

                declare ptr @ptr_to_string(i64, ptr, i64)

                define ptr @main() {
                start:
                  %alloca_a = alloca ptr, align 8
                  store ptr @const_string, ptr %alloca_a, align 8
                  %load_a = load ptr, ptr %alloca_a, align 8
                  ret ptr %load_a
                }

                define ptr @__main__() {
                start:
                  %call_entry_point = call ptr @main()
                  ret void
                }
            "#]],
        );
    }

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
                  ret i64 30
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
                  ret i64 10
                }

                define i64 @func() {
                start:
                  ret i64 20
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
                  %alloca_x = alloca i64, align 8
                  store i64 10, ptr %alloca_x, align 8
                  %load_x = load i64, ptr %alloca_x, align 8
                  ret i64 %load_x
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

    #[test]
    fn test_return_string_of_main() {
        check_result(
            r#"
            fn main() -> string {
                "aaa"
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "String",
                  "value": "aaa"
                }
            "#]],
        );
    }

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

        check_result(
            r#"
            fn main() -> string {
                let x = "aaa"
                x
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "String",
                  "value": "aaa"
                }
            "#]],
        );
    }

    #[test]
    fn test_multiple_ref() {
        check_result(
            r#"
            fn main() -> string {
                let x = "aaa"
                let y = x
                let z = y
                z
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "String",
                  "value": "aaa"
                }
            "#]],
        );
    }

    #[test]
    fn test_block() {
        check_result(
            r#"
            fn main() {
                {
                }
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
    fn test_block_binding() {
        check_result(
            r#"
            fn main() {
                let a = {
                }
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Unit",
                  "value": null
                }
            "#]],
        );

        check_result(
            r#"
            fn main() -> int {
                let a = {
                    10
                }
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

    #[test]
    fn test_call_with_params() {
        check_result(
            r#"
            fn test(x: int) -> int {
                x
            }

            fn main() -> int {
                let a = test(10)
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

        check_result(
            r#"
            fn test(x: int, y: int) -> int {
                y
            }

            fn main() -> int {
                let a = test(10, 20)
                a
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 20
                }
            "#]],
        );
    }

    #[test]
    fn test_call_with_param_types() {
        check_result(
            r#"
            fn test(x: int) -> int {
                x
            }

            fn main() -> int {
                let a = test(10)
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

        check_result(
            r#"
            fn test(x: string) -> string {
                x
            }

            fn main() -> string {
                let a = test("aaa")
                a
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "String",
                  "value": "aaa"
                }
            "#]],
        );

        check_result(
            r#"
            fn test(x: bool) -> bool {
                x
            }

            fn main() -> bool {
                let a = test(true)
                a
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
    fn test_gen_signature_preorder() {
        check_result(
            r#"
            fn main() -> bool {
                let a = test1(true)
                a
            }

            fn test3(x: bool) -> bool {
                x
            }

            fn test1(x: bool) -> bool {
                test2(x)
            }

            fn test2(x: bool) -> bool {
                test3(x)
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

    #[test]
    fn test_add_number() {
        check_result(
            r#"
            fn main() -> int {
                let a = 10
                let b = 20
                let c = {
                    a + 30
                }
                b + c + 40
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

    #[test]
    fn test_sub_number() {
        check_result(
            r#"
            fn main() -> int {
                let a = 10
                let b = 20
                a - b - 30
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": -40
                }
            "#]],
        );
    }

    #[test]
    fn test_mul_number() {
        check_result(
            r#"
            fn main() -> int {
                let a = 10
                let b = 20
                a * b * 30
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 6000
                }
            "#]],
        );
    }

    #[test]
    fn test_div_number() {
        check_result(
            r#"
            fn main() -> int {
                let a = 10
                let b = 20
                2000 / a / b
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

    #[test]
    fn test_div_number_truncation() {
        check_result(
            r#"
            fn main() -> int {
                let a = 20
                let b = 3
                a / b
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 6
                }
            "#]],
        );
    }

    #[test]
    fn test_negative_number() {
        check_result(
            r#"
            fn main() -> int {
                let a = -10
                let b = 20
                let c = -30
                let d = -{
                    40
                }
                a + (-b) + (-c) - d
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 40
                }
            "#]],
        );
    }

    #[test]
    fn test_equal_number() {
        check_result(
            r#"
            fn main() -> bool {
                let a = 10
                let b = 10
                a == b
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Boolean",
                  "value": true
                }
            "#]],
        );

        check_result(
            r#"
            fn main() -> bool {
                let a = 10
                let b = 20
                a == b
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Boolean",
                  "value": false
                }
            "#]],
        );
    }
}
