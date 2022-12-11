use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
};

pub fn codegen<'a, 'ctx>(
    hir_result: &'a hir::LowerResult,
    ty_result: &'a hir_ty::TyLowerResult,
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    execution_engine: &'a ExecutionEngine<'ctx>,
) -> CodegenResult<'ctx> {
    let codegen = Codegen {
        hir_result,
        ty_result,
        context,
        module,
        builder,
        execution_engine,
    };
    codegen.gen()
}

type MainFunc = unsafe extern "C" fn() -> *mut i8;
pub struct CodegenResult<'ctx> {
    function: JitFunction<'ctx, MainFunc>,
}

struct Codegen<'a, 'ctx> {
    hir_result: &'a hir::LowerResult,
    ty_result: &'a hir_ty::TyLowerResult,

    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    execution_engine: &'a ExecutionEngine<'ctx>,
}

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    fn gen(mut self) -> CodegenResult<'ctx> {
        if self.hir_result.entry_point.is_none() {
            unimplemented!();
        }

        let fn_type = self.context.void_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let entry_point = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry_point);
        self.builder.build_return(None);

        CodegenResult {
            function: unsafe { self.execution_engine.get_function("main").unwrap() },
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::AstNode;
    use expect_test::{expect, Expect};
    use inkwell::OptimizationLevel;

    use super::*;

    fn check(input: &str, expect: Expect) {
        let parsed = parser::parse(input);
        let ast = ast::SourceFile::cast(parsed.syntax()).unwrap();
        let hir_result = hir::lower(ast);
        let ty_result = hir_ty::lower(&hir_result);
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
        );

        expect.assert_eq(&module.to_string());
    }

    #[test]
    fn ir() {
        check(
            r#"
            fn main() {
                10
            }
        "#,
            expect![[r#"
                ; ModuleID = 'top'
                source_filename = "top"
                target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"

                define void @main() {
                entry:
                  ret void
                }
            "#]],
        );
    }
}
