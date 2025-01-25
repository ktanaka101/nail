use std::sync::atomic::AtomicI64;

use inkwell::module::Linkage;

use crate::Codegen;

#[derive(Debug)]
pub(crate) struct StackMapIdGenerator {
    id: AtomicI64,
}
impl StackMapIdGenerator {
    pub fn new() -> Self {
        Self {
            id: AtomicI64::new(0),
        }
    }

    pub fn next(&self) -> i64 {
        self.id.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    }
}

pub(super) fn define_stackmap_functions(codegen: &mut Codegen) {
    define_llvm_stackmap(codegen);
}

const NAME: &str = "llvm.experimental.stackmap";
fn define_llvm_stackmap(codegen: &mut Codegen) {
    let return_ty = codegen.context.void_type();
    let fn_type = return_ty.fn_type(
        &[
            codegen.context.i64_type().into(),
            codegen.context.i32_type().into(),
        ],
        true,
    );
    let func_value = codegen
        .module
        .add_function(NAME, fn_type, Some(Linkage::External));
    codegen
        .builtin_functions
        .insert(NAME.to_string(), func_value);
}

impl<'ctx> Codegen<'_, 'ctx> {
    pub(crate) fn build_llvm_stackmap(
        &self,
        id: inkwell::values::IntValue<'ctx>,
        num_shadow_bytes: inkwell::values::IntValue<'ctx>,
        extra_args: &[inkwell::values::BasicMetadataValueEnum<'ctx>],
    ) {
        let fn_llvm_stackmap = self
            .module
            .get_function(NAME)
            .unwrap_or_else(|| panic!("{NAME} not declared"));
        self.builder
            .build_call(
                fn_llvm_stackmap,
                &[&[id.into(), num_shadow_bytes.into()], extra_args].concat(),
                &format!("call_{NAME}"),
            )
            .unwrap();
    }
}
