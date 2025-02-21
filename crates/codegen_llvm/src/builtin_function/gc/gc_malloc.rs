use inkwell::{types::BasicType, values::PointerValue, AddressSpace};

use crate::Codegen;

const FN_NAME: &str = "nail_gc_malloc";
pub(super) fn define_gc_malloc(codegen: &mut Codegen) {
    let return_ty = codegen.context.ptr_type(AddressSpace::default());
    let fn_type = return_ty.fn_type(
        &[
            codegen.context.i64_type().into(), // size
            codegen.context.i8_type().into(),  // type_tag
        ],
        false,
    );
    let func_value = codegen.module.add_function(FN_NAME, fn_type, None);
    codegen
        .execution_engine
        .add_global_mapping(&func_value, nail_gc::externs::nail_gc_malloc as usize);
    codegen
        .builtin_functions
        .insert(FN_NAME.to_string(), func_value);
}

impl<'ctx> Codegen<'_, 'ctx> {
    pub(crate) fn build_call_gc_malloc<T: BasicType<'ctx>>(&self, ty: T) -> PointerValue<'ctx> {
        if !ty.is_sized() {
            panic!("Cannot allocate unsized type");
        }

        let fn_gc_collect = self
            .module
            .get_function(FN_NAME)
            .unwrap_or_else(|| panic!("{FN_NAME} not declared"));
        let size = ty.size_of().unwrap();

        let stackmap_id = self.stackmap_generator.next();
        let stackmap_id = self.context.i64_type().const_int(stackmap_id as u64, false);
        self.build_llvm_stackmap(stackmap_id, self.context.i32_type().const_zero(), &[]);

        // TODO: type_tag
        self.builder
            .build_call(
                fn_gc_collect,
                &[
                    size.into(),
                    self.context.i8_type().const_int(0, false).into(), // Default to Integer type
                ],
                &format!("call_{FN_NAME}"),
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value()
    }
}
