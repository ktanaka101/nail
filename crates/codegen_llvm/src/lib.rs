mod builtin_function;

use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    types::{BasicMetadataTypeEnum, FunctionType},
    values::FunctionValue,
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
        };
        codegen.add_builtin_function();

        codegen
    }

    fn gen(mut self, should_return_string: bool) -> CodegenResult<'ctx> {
        if self.hir_result.entry_point.is_none() {
            unimplemented!();
        }

        self.gen_functions();

        let fn_type = self.context.i8_type().fn_type(&[], false);
        let main_fn = self
            .module
            .add_function(INTERNAL_ENTRY_POINT, fn_type, None);
        let inner_entry_point_block = self
            .context
            .append_basic_block(main_fn, FN_ENTRY_BLOCK_NAME);
        self.builder.position_at_end(inner_entry_point_block);

        let entry_point = self
            .defined_functions
            .get(&self.hir_result.entry_point.unwrap())
            .unwrap();
        self.builder
            .build_call(*entry_point, &[], "call_entry_point");

        let int = self.context.i64_type().const_int(10, false);
        let ptr = self.builder.build_alloca(int.get_type(), "alloca_i");
        self.builder.build_store(ptr, int);

        if should_return_string {
            let call_v = self.build_call_ptr_to_string(ptr);
            let return_v = call_v.try_as_basic_value().left().unwrap();

            self.builder.build_return(Some(&return_v));
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

    fn gen_functions(&mut self) {
        for (idx, function) in self.hir_result.db.functions.iter() {
            let signature = self.ty_result.signature_by_function(&idx);

            let fn_ty: FunctionType<'ctx> = match signature.return_type {
                hir_ty::ResolvedType::Unit => {
                    let ty = self.context.void_type();
                    let params = signature
                        .params
                        .iter()
                        .map(|param| match param {
                            hir_ty::ResolvedType::Integer => {
                                BasicMetadataTypeEnum::IntType(self.context.i64_type())
                            }
                            _ => unimplemented!(),
                        })
                        .collect::<Vec<_>>();
                    ty.fn_type(&params, false)
                }
                _ => unimplemented!(),
            };

            let function_name = self
                .hir_result
                .interner
                .lookup(function.name.unwrap().key());
            let function = self.module.add_function(function_name, fn_ty, None);
            self.defined_functions.insert(idx, function);

            let start_block = self
                .context
                .append_basic_block(function, FN_ENTRY_BLOCK_NAME);
            self.builder.position_at_end(start_block);

            let body_block = self.hir_result.function_body_by_function(&idx).unwrap();
            match body_block {
                hir::Expr::Block(block) => self.gen_body(block),
                _ => unreachable!(),
            }

            self.builder.build_return(None);
        }
    }

    fn gen_body(&self, block: &hir::Block) {
        for stmt in &block.stmts {
            self.gen_stmt(stmt);
        }
        // stmt.tail
    }

    fn gen_stmt(&self, stmt: &hir::Stmt) {
        match stmt {
            hir::Stmt::Expr(expr) => {
                let expr = &self.hir_result.shared_ctx.exprs[*expr];
                match expr {
                    hir::Expr::Literal(literal) => match literal {
                        hir::Literal::Integer(integer) => {
                            todo!()
                        }
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
        todo!()
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
        let result_string = {
            let c_string_ptr = unsafe { result.function.call() };
            unsafe { CString::from_raw(c_string_ptr as *mut c_char) }
                .into_string()
                .unwrap()
        };
        expect.assert_eq(&result_string);
    }

    #[test]
    fn ir() {
        check_ir(
            r#"
            fn main() {
                10
            }
        "#,
            expect![[r#"
                ; ModuleID = 'top'
                source_filename = "top"

                declare i8* @ptr_to_string(i64*, i64)

                define void @main() {
                start:
                  ret void
                }

                define i8 @__main__() {
                start:
                  call void @main()
                  %alloca_i = alloca i64, align 8
                  store i64 10, i64* %alloca_i, align 8
                  ret void
                }
            "#]],
        );
    }

    #[test]
    fn test_gen_functions() {
        check_ir(
            r#"
            fn main() {
                10
            }
            fn func() {
                20
            }
        "#,
            expect![[r#"
                ; ModuleID = 'top'
                source_filename = "top"

                declare i8* @ptr_to_string(i64*, i64)

                define void @main() {
                start:
                  ret void
                }

                define void @func() {
                start:
                  ret void
                }

                define i8 @__main__() {
                start:
                  call void @main()
                  %alloca_i = alloca i64, align 8
                  store i64 10, i64* %alloca_i, align 8
                  ret void
                }
            "#]],
        );
    }

    #[test]
    fn result() {
        check_result(
            r#"
            fn main() {
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
