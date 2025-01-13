//! Garbage Collection functions.

use inkwell::{
    builder::BuilderError,
    values::{IntValue, PointerValue},
};

use crate::Codegen;

impl<'ctx> Codegen<'_, 'ctx> {
    pub(crate) fn declare_gc_functions(&self) {
        // void GC_init(void);
        let fn_gc_init_ty = self.context.void_type().fn_type(&[], false);
        self.module.add_function("GC_init", fn_gc_init_ty, None);

        // i8* GC_malloc(i64);
        let i8_ptr = self.context.ptr_type(Default::default());
        let i64_ty = self.context.i64_type();
        let fn_gc_malloc_ty = i8_ptr.fn_type(&[i64_ty.into()], false);
        self.module.add_function("GC_malloc", fn_gc_malloc_ty, None);

        // void GC_free(i8*);
        let fn_gc_free_ty = self.context.void_type().fn_type(&[i8_ptr.into()], false);
        self.module.add_function("GC_free", fn_gc_free_ty, None);

        // i8* GC_realloc(i8*, i64);
        let fn_gc_realloc_ty = i8_ptr.fn_type(&[i8_ptr.into(), i64_ty.into()], false);
        self.module
            .add_function("GC_realloc", fn_gc_realloc_ty, None);

        // void GC_collect(void);
        let fn_gc_collect_ty = self.context.void_type().fn_type(&[], false);
        self.module
            .add_function("GC_collect", fn_gc_collect_ty, None);
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

    /// Call `i8* GC_malloc(size: i64)` function.
    pub(crate) fn build_call_gc_malloc(
        &self,
        size: IntValue<'ctx>,
    ) -> Result<PointerValue<'ctx>, BuilderError> {
        let call_site = self.builder.build_call(
            self.module
                .get_function("GC_malloc")
                .expect("GC_malloc not declared"),
            &[size.into()],
            "call_gc_malloc",
        )?;
        // Cast i8* to BasicValueEnum::PointerValue
        let returned = call_site
            .try_as_basic_value()
            .left()
            .expect("GC_malloc call did not return a pointer");
        Ok(returned.into_pointer_value())
    }

    /// Call `GC_free(ptr: i8*)` function.
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

    /// Call `i8* GC_realloc(old_ptr: i8*, new_size: i64)` function.
    pub(crate) fn build_call_gc_realloc(
        &self,
        old_ptr: PointerValue<'ctx>,
        new_size: IntValue<'ctx>,
    ) -> Result<PointerValue<'ctx>, BuilderError> {
        let call_site = self.builder.build_call(
            self.module
                .get_function("GC_realloc")
                .expect("GC_realloc not declared"),
            &[old_ptr.into(), new_size.into()],
            "call_gc_realloc",
        )?;
        // Cast i8* to BasicValueEnum::PointerValue
        let returned = call_site
            .try_as_basic_value()
            .left()
            .expect("GC_realloc call did not return a pointer");
        Ok(returned.into_pointer_value())
    }

    /// Call `GC_collect()` function.
    pub(crate) fn build_call_gc_collect(&self) -> Result<(), BuilderError> {
        self.builder.build_call(
            self.module
                .get_function("GC_collect")
                .expect("GC_collect not declared"),
            &[],
            "call_gc_collect",
        )?;
        Ok(())
    }
}
