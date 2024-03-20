//! LLVM IRを生成するモジュールです。

mod body;
mod builtin_function;
use std::collections::HashMap;

use body::BodyCodegen;
pub use builtin_function::{Output, OutputType};
use either::Either;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    types::{BasicMetadataTypeEnum, FunctionType, IntType, PointerType, StructType},
    values::{FunctionValue, PointerValue},
    AddressSpace,
};

const FN_ENTRY_BLOCK_NAME: &str = "start";
const INTERNAL_ENTRY_POINT: &str = "__main__";

/// LLVM IR生成用のコンテキストです。
pub struct CodegenContext<'a, 'ctx> {
    /// LLVMのコンテキスト
    pub context: &'ctx Context,
    /// LLVMのモジュール
    pub module: &'a Module<'ctx>,
    /// LLVMのビルダー
    pub builder: &'a Builder<'ctx>,
    /// LLVMの実行エンジン
    pub execution_engine: &'a ExecutionEngine<'ctx>,
}

/// LLVM IRを生成します。
pub fn codegen<'a, 'ctx>(
    db: &'a dyn hir::HirMasterDatabase,
    mir_result: &'a mir::LowerResult,
    codegen_context: &'a CodegenContext<'a, 'ctx>,
    should_return_string: bool,
) -> CodegenResult<'ctx> {
    let codegen = Codegen::new(
        db,
        mir_result,
        codegen_context.context,
        codegen_context.module,
        codegen_context.builder,
        codegen_context.execution_engine,
    );
    codegen.gen(should_return_string)
}

/// エントリポイントの関数の型です。
type MainFunc = unsafe extern "C" fn() -> *mut i8;

/// LLVM IRの生成結果です。
pub struct CodegenResult<'ctx> {
    /// エントリポイントの関数
    pub function: JitFunction<'ctx, MainFunc>,
}

struct Codegen<'a, 'ctx> {
    db: &'a dyn hir::HirMasterDatabase,
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
        db: &'a dyn hir::HirMasterDatabase,
        mir_result: &'a mir::LowerResult,
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        execution_engine: &'a ExecutionEngine<'ctx>,
    ) -> Self {
        let mut codegen = Self {
            db,
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

        self.gen_function_signatures(self.db);
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
            self.builder
                .build_call(
                    self.defined_functions[&self.mir_result.function_id_of_entry_point().unwrap()],
                    &[],
                    "call_entry_point",
                )
                .unwrap()
        };

        if should_return_string {
            match result.try_as_basic_value() {
                Either::Left(result_value) => {
                    let call_v = self.build_to_string(result_value);
                    let return_v = call_v.try_as_basic_value().left().unwrap();

                    self.builder.build_return(Some(&return_v)).unwrap();
                }
                Either::Right(_) => unreachable!(),
            }
        } else {
            self.builder.build_return(None).unwrap();
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
                hir_ty::Monotype::Integer => {
                    BasicMetadataTypeEnum::IntType(self.context.i64_type())
                }
                hir_ty::Monotype::String => self
                    .context
                    .i8_type()
                    .vec_type(1)
                    .ptr_type(AddressSpace::default())
                    .into(),
                hir_ty::Monotype::Bool => self.context.bool_type().into(),
                _ => unimplemented!(),
            })
            .collect::<Vec<_>>()
    }

    fn lookup_name(&self, db: &'a dyn hir::HirMasterDatabase, name: &hir::Name) -> &'a str {
        name.text(db)
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
            .unwrap()
            .as_pointer_value()
    }

    fn gen_function_signatures(&mut self, db: &dyn hir::HirMasterDatabase) {
        for (idx, body) in self.mir_result.ref_bodies() {
            let params = self.body_to_params(body);
            let return_type = body.locals[body.return_local].ty.clone();

            let fn_ty: FunctionType<'ctx> = match return_type {
                hir_ty::Monotype::Unit => {
                    let ty = self.context.struct_type(&[], false);
                    ty.fn_type(&params, false)
                }
                hir_ty::Monotype::Integer => {
                    let ty = self.context.i64_type();
                    ty.fn_type(&params, false)
                }
                hir_ty::Monotype::String => {
                    let ty = self.string_type();
                    ty.fn_type(&params, false)
                }
                hir_ty::Monotype::Bool => {
                    let ty = self.context.bool_type();
                    ty.fn_type(&params, false)
                }
                _ => unimplemented!(),
            };

            let function_name = self.lookup_name(db, &body.name);
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
}

#[cfg(test)]
mod tests {
    use std::ffi::{c_char, CString};

    use expect_test::{expect, Expect};
    use hir::FixtureDatabase;
    use inkwell::OptimizationLevel;

    use super::*;

    fn lower(
        fixture: &str,
    ) -> (
        hir_ty::TestingDatabase,
        hir::Pods,
        hir_ty::TyLowerResult,
        mir::LowerResult,
    ) {
        let db = hir_ty::TestingDatabase::default();
        let source_db = FixtureDatabase::new(&db, fixture);

        let pods = hir::parse_pods(&db, &source_db);
        let ty_result = hir_ty::lower_pods(&db, &pods);
        let mir_result = mir::lower_pods(&db, &pods, &ty_result);

        (db, pods, ty_result, mir_result)
    }

    fn execute_in_root_file(fixture: &str) -> String {
        let mut fixture = fixture.to_string();
        fixture.insert_str(0, "//- /main.nail\n");

        let (db, _pods, _ty_result, mir_result) = lower(&fixture);

        let context = Context::create();
        let module = context.create_module("top");
        let builder = context.create_builder();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let result = codegen(
            &db,
            &mir_result,
            &CodegenContext {
                context: &context,
                module: &module,
                builder: &builder,
                execution_engine: &execution_engine,
            },
            true,
        );
        module.print_to_stderr();

        {
            let c_string_ptr = unsafe { result.function.call() };
            unsafe { CString::from_raw(c_string_ptr as *mut c_char) }
                .into_string()
                .unwrap()
        }
    }

    fn check_ir_in_root_file(fixture: &str, expect: Expect) {
        let mut fixture = fixture.to_string();
        fixture.insert_str(0, "//- /main.nail\n");

        check_pod_result_start_with_root_file(&fixture, expect);
    }

    fn check_pod_result_start_with_root_file(fixture: &str, expect: Expect) {
        let (db, _pods, _ty_result, mir_result) = lower(fixture);

        let context = Context::create();
        let module = context.create_module("top");
        let builder = context.create_builder();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        codegen(
            &db,
            &mir_result,
            &CodegenContext {
                context: &context,
                module: &module,
                builder: &builder,
                execution_engine: &execution_engine,
            },
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

    fn check_result_in_root_file(fixture: &str, expect: Expect) {
        let result_string = execute_in_root_file(fixture);
        expect.assert_eq(&result_string);
    }

    fn check_integer(input: &str, expect: i64) {
        let input_with_entry_point = format!(
            r#"
                fn main() -> int {{
                    {input}
                }}
            "#
        );

        let result_string = execute_in_root_file(&input_with_entry_point);

        let output = serde_json::from_str::<crate::Output>(&result_string).unwrap();
        assert_eq!(
            output,
            crate::Output {
                nail_type: crate::OutputType::Int,
                value: serde_json::json!(expect)
            },
            "input: {input}"
        );
    }

    fn check_bool(input: &str, expect: bool) {
        let input_with_entry_point = format!(
            r#"
                fn main() -> bool {{
                    {input}
                }}
            "#
        );

        let result_string = execute_in_root_file(&input_with_entry_point);

        let output = serde_json::from_str::<crate::Output>(&result_string).unwrap();
        assert_eq!(
            output,
            crate::Output {
                nail_type: crate::OutputType::Boolean,
                value: serde_json::json!(expect)
            },
            "input: {input}",
        );
    }

    #[test]
    fn test_ir_fibonacci() {
        check_ir_in_root_file(
            r#"
                fn fibonacci(x: int) -> int {
                    if x == 0 {
                        0
                    } else {
                        if x == 1 {
                            1
                        } else {
                            fibonacci(x - 1) + fibonacci(x - 2)
                        }
                    }
                }
                fn main() -> int {
                    fibonacci(15)
                }
            "#,
            expect![[r#"
                ; ModuleID = 'top'
                source_filename = "top"

                declare ptr @ptr_to_string(i64, ptr, i64)

                define i64 @fibonacci(i64 %0) {
                start:
                  %"0" = alloca i64, align 8
                  %"2" = alloca i1, align 1
                  %"3" = alloca i1, align 1
                  %"4" = alloca i64, align 8
                  %"5" = alloca i1, align 1
                  %"6" = alloca i1, align 1
                  %"7" = alloca i64, align 8
                  %"8" = alloca i64, align 8
                  %"9" = alloca i64, align 8
                  %"10" = alloca i64, align 8
                  %"11" = alloca i64, align 8
                  %"12" = alloca i64, align 8
                  br label %entry

                entry:                                            ; preds = %start
                  %compare_number = icmp eq i64 %0, 0
                  store i1 %compare_number, ptr %"3", align 1
                  %load = load i1, ptr %"3", align 1
                  store i1 %load, ptr %"2", align 1
                  %load1 = load i1, ptr %"2", align 1
                  switch i1 %load1, label %else0 [
                    i1 true, label %then0
                  ]

                exit:                                             ; preds = %bb0
                  %load2 = load i64, ptr %"0", align 8
                  ret i64 %load2

                bb0:                                              ; preds = %bb1, %then0
                  %load3 = load i64, ptr %"4", align 8
                  store i64 %load3, ptr %"0", align 8
                  br label %exit

                then0:                                            ; preds = %entry
                  store i64 0, ptr %"4", align 8
                  br label %bb0

                else0:                                            ; preds = %entry
                  %compare_number4 = icmp eq i64 %0, 1
                  store i1 %compare_number4, ptr %"6", align 1
                  %load5 = load i1, ptr %"6", align 1
                  store i1 %load5, ptr %"5", align 1
                  %load6 = load i1, ptr %"5", align 1
                  switch i1 %load6, label %else1 [
                    i1 true, label %then1
                  ]

                bb1:                                              ; preds = %bb3, %then1
                  %load7 = load i64, ptr %"7", align 8
                  store i64 %load7, ptr %"4", align 8
                  br label %bb0

                then1:                                            ; preds = %else0
                  store i64 1, ptr %"7", align 8
                  br label %bb1

                else1:                                            ; preds = %else0
                  %sub_number = sub i64 %0, 1
                  store i64 %sub_number, ptr %"8", align 8
                  %load8 = load i64, ptr %"8", align 8
                  %call = call i64 @fibonacci(i64 %load8)
                  store i64 %call, ptr %"9", align 8
                  br label %bb2

                bb2:                                              ; preds = %else1
                  %sub_number9 = sub i64 %0, 2
                  store i64 %sub_number9, ptr %"10", align 8
                  %load10 = load i64, ptr %"10", align 8
                  %call11 = call i64 @fibonacci(i64 %load10)
                  store i64 %call11, ptr %"11", align 8
                  br label %bb3

                bb3:                                              ; preds = %bb2
                  %load12 = load i64, ptr %"9", align 8
                  %load13 = load i64, ptr %"11", align 8
                  %add_number = add i64 %load12, %load13
                  store i64 %add_number, ptr %"12", align 8
                  %load14 = load i64, ptr %"12", align 8
                  store i64 %load14, ptr %"7", align 8
                  br label %bb1
                }

                define i64 @main() {
                start:
                  %"0" = alloca i64, align 8
                  %"1" = alloca i64, align 8
                  br label %entry

                entry:                                            ; preds = %start
                  %call = call i64 @fibonacci(i64 15)
                  store i64 %call, ptr %"1", align 8
                  br label %bb0

                exit:                                             ; preds = %bb0
                  %load = load i64, ptr %"0", align 8
                  ret i64 %load

                bb0:                                              ; preds = %entry
                  %load1 = load i64, ptr %"1", align 8
                  store i64 %load1, ptr %"0", align 8
                  br label %exit
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
    fn test_ir_literals() {
        check_ir_in_root_file(
            r#"
            fn main() -> int {
                let a = "aaa";
                let b = 10;
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
                  %"0" = alloca i64, align 8
                  %"1" = alloca ptr, align 8
                  %"2" = alloca i64, align 8
                  br label %entry

                entry:                                            ; preds = %start
                  store ptr @const_string, ptr %"1", align 8
                  store i64 10, ptr %"2", align 8
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
    fn test_ir_return_string() {
        check_ir_in_root_file(
            r#"
            fn main() -> string {
                let a = "aaa";
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
                  %"0" = alloca ptr, align 8
                  %"1" = alloca ptr, align 8
                  br label %entry

                entry:                                            ; preds = %start
                  store ptr @const_string, ptr %"1", align 8
                  %load = load ptr, ptr %"1", align 8
                  store ptr %load, ptr %"0", align 8
                  br label %exit

                exit:                                             ; preds = %entry
                  %load1 = load ptr, ptr %"0", align 8
                  ret ptr %load1
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
        check_ir_in_root_file(
            r#"
            fn main() -> int {
                10;
                20;
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
        check_ir_in_root_file(
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
        check_ir_in_root_file(
            r#"
            fn main() -> int {
                let x = 10;
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
    fn test_fibonacci() {
        check_result_in_root_file(
            r#"
                fn fibonacci(x: int) -> int {
                    if x == 0 {
                        0
                    } else {
                        if x == 1 {
                            1
                        } else {
                            fibonacci(x - 1) + fibonacci(x - 2)
                        }
                    }
                }
                fn main() -> int {
                    fibonacci(15)
                }
            "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 610
                }
            "#]],
        );
    }

    #[test]
    fn test_return_integer_of_main() {
        check_result_in_root_file(
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
        check_result_in_root_file(
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
        check_result_in_root_file(
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
        check_result_in_root_file(
            r#"
            fn main() {
                let a = 10;
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
        check_result_in_root_file(
            r#"
            fn main() -> int {
                let x = 10;
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

        check_result_in_root_file(
            r#"
            fn main() -> string {
                let x = "aaa";
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
        check_result_in_root_file(
            r#"
            fn main() -> string {
                let x = "aaa";
                let y = x;
                let z = y;
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
        check_result_in_root_file(
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
        check_result_in_root_file(
            r#"
            fn main() {
                let a = {
                };
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Unit",
                  "value": null
                }
            "#]],
        );

        check_result_in_root_file(
            r#"
            fn main() -> int {
                let a = {
                    10
                };
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
        check_result_in_root_file(
            r#"
            fn test() -> int {
                10
            }

            fn main() -> int {
                let a = test();
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
        check_result_in_root_file(
            r#"
            fn test(x: int) -> int {
                x
            }

            fn main() -> int {
                let a = test(10);
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

        check_result_in_root_file(
            r#"
            fn test(x: int, y: int) -> int {
                y
            }

            fn main() -> int {
                let a = test(10, 20);
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
        check_result_in_root_file(
            r#"
            fn test(x: int) -> int {
                x
            }

            fn main() -> int {
                let a = test(10);
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

        check_result_in_root_file(
            r#"
            fn test(x: string) -> string {
                x
            }

            fn main() -> string {
                let a = test("aaa");
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

        check_result_in_root_file(
            r#"
            fn test(x: bool) -> bool {
                x
            }

            fn main() -> bool {
                let a = test(true);
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
        check_result_in_root_file(
            r#"
            fn main() -> bool {
                let a = test1(true);
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
        check_result_in_root_file(
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

        check_result_in_root_file(
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

        check_result_in_root_file(
            r#"
            fn main() -> int {
                let a = true;
                let b = 10;
                let c = 20;
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
    fn test_if_cond_by_comparing() {
        check_integer("if 5 > 2 { 10 } else { 20 }", 10);
        check_integer("if 2 > 5 { 10 } else { 20 }", 20);
        check_integer("if 5 < 8 { 10 } else { 20 }", 10);
        check_integer("if 8 < 5 { 10 } else { 20 }", 20);
        check_integer("if 5 == 5 { 10 } else { 20 }", 10);
        check_integer("if 5 == 6 { 10 } else { 20 }", 20);
    }

    #[test]
    fn test_add_number() {
        check_result_in_root_file(
            r#"
            fn main() -> int {
                let a = 10;
                let b = 20;
                let c = {
                    a + 30
                };
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
        check_result_in_root_file(
            r#"
            fn main() -> int {
                let a = 10;
                let b = 20;
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
        check_result_in_root_file(
            r#"
            fn main() -> int {
                let a = 10;
                let b = 20;
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
        check_result_in_root_file(
            r#"
            fn main() -> int {
                let a = 10;
                let b = 20;
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
        check_result_in_root_file(
            r#"
            fn main() -> int {
                let a = 20;
                let b = 3;
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
    fn test_equal_number() {
        check_result_in_root_file(
            r#"
            fn main() -> bool {
                let a = 1;
                let b = 1;
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

        check_result_in_root_file(
            r#"
            fn main() -> bool {
                let a = 1;
                let b = 3;
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

    #[test]
    fn test_equal_bool() {
        check_result_in_root_file(
            r#"
            fn main() -> bool {
                let a = true;
                let b = true;
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

        check_result_in_root_file(
            r#"
            fn main() -> bool {
                let a = true;
                let b = false;
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

    #[test]
    fn test_ord_number() {
        check_result_in_root_file(
            r#"
            fn main() -> bool {
                let a = 1;
                let b = 2;
                a < b
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Boolean",
                  "value": true
                }
            "#]],
        );

        check_result_in_root_file(
            r#"
            fn main() -> bool {
                let a = 1;
                let b = 2;
                a > b
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Boolean",
                  "value": false
                }
            "#]],
        );

        check_result_in_root_file(
            r#"
            fn main() -> bool {
                let a = 2;
                let b = 1;
                a < b
            }
        "#,
            expect![[r#"
                {
                  "nail_type": "Boolean",
                  "value": false
                }
            "#]],
        );

        check_result_in_root_file(
            r#"
            fn main() -> bool {
                let a = 2;
                let b = 1;
                a > b
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
    fn test_number_formula() {
        check_integer("5 + 5 + 5 + 5 - 10", 10);
        check_integer("2 * 2 * 2 * 2 * 2", 32);
        check_integer("-50 + 100 + -50", 0);
        check_integer("5 * 2 + 10", 20);
        check_integer("5 + 2 * 10", 25);
        check_integer("20 + 2 * -10", 0);
        check_integer("50 / 2 * 2 + 10", 60);
        check_integer("2 * (5 + 10)", 30);
        check_integer("3 * 3 * 3 + 10", 37);
        check_integer("3 * (3 * 3) + 10", 37);
        check_integer("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50);
    }

    #[test]
    fn test_compare() {
        check_bool("0 > 0", false);
        check_bool("0 < 0", false);
        check_bool("0 > 1", false);
        check_bool("0 < 1", true);
        check_bool("0 == 0", true);
        check_bool("0 == 1", false);
        check_bool("let a = 10; a > 0", true);
        check_bool("let a = 10; a < 0", false);
        check_bool("let a = 10; (a + 10) < 10", false);
        check_bool("let a = 10; (a + 10) > 10", true);
        check_bool("let a = 10; (a == 10) == true", true);
        check_bool("let a = 10; (a == 10) == false", false);
        check_bool("let a = 10; (a == a) == true", true);
    }

    #[test]
    fn test_assign() {
        check_result_in_root_file(
            r#"
                fn main() -> int {
                    let a = 10;
                    a = 20;

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
    fn test_negative_number() {
        check_result_in_root_file(
            r#"
            fn main() -> int {
                let a = -10;
                let b = 20;
                let c = -30;
                let d = -{
                    40
                };
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
    fn test_not_bool() {
        check_result_in_root_file(
            r#"
            fn main() -> bool {
                let a = !{
                    let b = true;
                    b
                };
                a
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

    #[test]
    fn test_bang() {
        check_bool("!false", true);
        check_bool("!true", false);
        check_bool("!!false", false);
        check_bool("!!true", true);
        check_bool("let a = true; !a", false);
        check_bool("let a = true; !!a", true);
        check_bool("let a = false; !a", true);
        check_bool("let a = false; !!a", false);
    }

    #[test]
    fn test_return() {
        check_result_in_root_file(
            r#"
                fn main() -> bool {
                    return true;
                }
            "#,
            expect![[r#"
                {
                  "nail_type": "Boolean",
                  "value": true
                }
            "#]],
        );

        check_result_in_root_file(
            r#"
                fn main() -> int {
                    return 10;
                }
            "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 10
                }
            "#]],
        );

        check_result_in_root_file(
            r#"
                fn main() -> int {
                    if false {
                        return 10;
                    } else {
                        return 20;
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

        check_result_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        10
                    } else {
                        return 20;
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

        check_result_in_root_file(
            r#"
                fn main() -> int {
                    if false {
                        return 10;
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

        check_result_in_root_file(
            r#"
                fn main() -> int {
                    if true {
                        return 10;
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

        check_result_in_root_file(
            r#"
                fn main() -> int {
                    if false {
                        10
                    } else {
                        return 20;
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
    }

    #[test]
    fn test_loop() {
        check_result_in_root_file(
            r#"
                fn main() -> bool {
                    loop {
                        break true;
                    }
                }
            "#,
            expect![[r#"
                {
                  "nail_type": "Boolean",
                  "value": true
                }
            "#]],
        );

        check_result_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        break 10;
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
    fn test_loop_break_in_if() {
        check_result_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        if true {
                            break 10;
                        } else {
                            break 20;
                        }
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

        check_result_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        if false {
                            break 10;
                        } else {
                            break 20;
                        }
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

        check_result_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        if false {
                            10
                        } else {
                            break 20;
                        }
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

        check_result_in_root_file(
            r#"
                fn main() -> int {
                    loop {
                        if true {
                            break 10;
                        } else {
                            20
                        }
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
    fn test_loop_continue() {
        check_result_in_root_file(
            r#"
                fn main() -> int {
                    let i = 0;
                    loop {
                        if i == 0 {
                            i = i + 1;
                            continue;
                        } else {
                            break i;
                        }
                    }
                }
            "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 1
                }
            "#]],
        );
    }

    #[test]
    fn test_while() {
        check_result_in_root_file(
            r#"
                fn main() -> int {
                    let i = 1;
                    while i < 3 {
                        i = i + 1;
                    }

                    i
                }
            "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 3
                }
            "#]],
        );
    }

    #[test]
    fn test_modules() {
        check_result_in_root_file(
            r#"
                fn main() -> int {
                    let x = module_aaa::module_bbb::function_aaa();
                    let y = module_aaa::function_ccc();
                    x + y
                }
                mod module_aaa {
                    mod module_bbb {
                        fn function_aaa() -> int {
                            mod module_ccc {
                                fn function_bbb() -> int {
                                    10
                                }
                            }

                            module_ccc::function_bbb()
                        }
                    }

                    fn function_ccc() -> int {
                        20
                    }
                }
            "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 30
                }
            "#]],
        );
    }

    // unimplemented
    #[test]
    #[should_panic]
    fn test_use_in_file() {
        check_result_in_root_file(
            r#"
                use module_aaa::module_bbb::function_aaa;
                use module_bbb::function_bbb;
                fn main() -> int {
                    let x = function_aaa();
                    let y = function_bbb();
                    x + y
                }
                mod module_aaa {
                    mod module_bbb {
                        fn function_aaa() -> int {
                            10
                        }
                    }
                }
                mod module_bbb {
                    mod fn function_bbb() -> int {
                        20
                    }
                }
            "#,
            expect![[r#"
                {
                  "nail_type": "Int",
                  "value": 30
                }
            "#]],
        );
    }

    #[test]
    #[should_panic]
    fn nested_outline_module() {
        check_pod_result_start_with_root_file(
            r#"
                //- /main.nail
                mod mod_aaa;

                fn main() -> integer {
                    mod_aaa::fn_aaa();
                    mod_aaa::mod_bbb::fn_bbb()
                }

                //- /mod_aaa.nail
                mod mod_bbb;
                fn fn_aaa() -> integer {
                    mod_bbb::fn_bbb()
                }

                //- /mod_aaa/mod_bbb.nail
                fn fn_bbb() -> integer {
                    10
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
