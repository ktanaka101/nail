//! Garbage Collection functions.

mod libgc_wrapper;

use inkwell::{
    builder::BuilderError,
    values::{IntValue, PointerValue},
    AddressSpace,
};

use crate::Codegen;

impl<'ctx> Codegen<'_, 'ctx> {
    pub(crate) fn declare_gc_functions(&self) {
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // void gc_init();
        let fn_gc_init_ty = self.context.void_type().fn_type(&[], false);
        let func_value = self.module.add_function("GC_init", fn_gc_init_ty, None);
        self.execution_engine
            .add_global_mapping(&func_value, libgc_wrapper::gc_init as usize);

        // c_void* gc_malloc(i64);
        let i64_ty = self.context.i64_type();
        let fn_gc_malloc_ty = ptr_type.fn_type(&[i64_ty.into()], false);
        self.module.add_function("GC_malloc", fn_gc_malloc_ty, None);
        self.execution_engine
            .add_global_mapping(&func_value, libgc_wrapper::gc_malloc as usize);

        // void gc_free(c_void*);
        let fn_gc_free_ty = self.context.void_type().fn_type(&[ptr_type.into()], false);
        self.module.add_function("GC_free", fn_gc_free_ty, None);
        self.execution_engine
            .add_global_mapping(&func_value, libgc_wrapper::gc_free as usize);

        // c_void* gc_realloc(c_void*, i64);
        let fn_gc_realloc_ty = ptr_type.fn_type(&[ptr_type.into(), i64_ty.into()], false);
        self.module
            .add_function("GC_realloc", fn_gc_realloc_ty, None);
        self.execution_engine
            .add_global_mapping(&func_value, libgc_wrapper::gc_realloc as usize);
    }

    //--------------------------------------------------------------------------------
    //  GC functions
    //--------------------------------------------------------------------------------

    /// Call `GC_init()` function.
    pub(crate) fn build_call_gc_init(&self) -> Result<(), BuilderError> {
        self.builder.build_call(
            self.module
                .get_function("GC_init")
                .expect("GC_init not declared"),
            &[],
            "call_gc_init",
        )?;
        Ok(())
    }

    /// Call [libgc_wrapper::gc_malloc] (`c_void* GC_malloc(size: i64)`)
    pub(crate) fn build_call_gc_malloc(
        &self,
        byte_size: IntValue<'ctx>,
    ) -> Result<PointerValue<'ctx>, BuilderError> {
        let call_site = self.builder.build_call(
            self.module
                .get_function("GC_malloc")
                .expect("GC_malloc not declared"),
            &[byte_size.into()],
            "call_gc_malloc",
        )?;
        // Cast c_void* to BasicValueEnum::PointerValue
        let returned = call_site
            .try_as_basic_value()
            .left()
            .expect("GC_malloc call did not return a pointer");
        Ok(returned.into_pointer_value())
    }

    /// Call [libgc_wrapper::gc_free] (`GC_free(ptr: c_void*)`)
    pub(crate) fn build_call_gc_free(&self, ptr: PointerValue<'ctx>) -> Result<(), BuilderError> {
        self.builder.build_call(
            self.module
                .get_function("GC_free")
                .expect("GC_free not declared"),
            &[ptr.into()],
            "call_gc_free",
        )?;
        Ok(())
    }

    /// Call [libgc_wrapper::gc_realloc] (`c_void* GC_realloc(c_void*, i64)`)
    pub(crate) fn build_call_gc_realloc(
        &self,
        old_ptr: PointerValue<'ctx>,
        new_byte_size: IntValue<'ctx>,
    ) -> Result<PointerValue<'ctx>, BuilderError> {
        let call_site = self.builder.build_call(
            self.module
                .get_function("GC_realloc")
                .expect("GC_realloc not declared"),
            &[old_ptr.into(), new_byte_size.into()],
            "call_gc_realloc",
        )?;
        // Cast c_void* to BasicValueEnum::PointerValue
        let returned = call_site
            .try_as_basic_value()
            .left()
            .expect("GC_realloc call did not return a pointer");
        Ok(returned.into_pointer_value())
    }
}
