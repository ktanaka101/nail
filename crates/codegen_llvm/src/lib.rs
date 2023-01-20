mod builtin_function;

use std::collections::HashMap;

use either::Either;
use inkwell::{
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

    fn gen_functions(&mut self) {
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
            self.set_function(function);

            let start_block = self
                .context
                .append_basic_block(function, FN_ENTRY_BLOCK_NAME);
            self.builder.position_at_end(start_block);

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
            _ => unimplemented!(),
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
            _ => unimplemented!(),
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
    }
}
