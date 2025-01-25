use crate::Codegen;

const NAME_GC_COLLECT: &str = "nail_gc_collect";
pub(super) fn define_gc_collect(codegen: &mut Codegen) {
    let return_ty = codegen.context.void_type();
    let fn_type = return_ty.fn_type(&[], false);
    let func_value = codegen.module.add_function(NAME_GC_COLLECT, fn_type, None);
    codegen
        .execution_engine
        .add_global_mapping(&func_value, nail_gc::externs::nail_gc_collect as usize);
    codegen
        .builtin_functions
        .insert(NAME_GC_COLLECT.to_string(), func_value);
}

impl Codegen<'_, '_> {
    pub(crate) fn build_call_gc_collect(&self) {
        let fn_gc_collect = self
            .module
            .get_function(NAME_GC_COLLECT)
            .unwrap_or_else(|| panic!("{NAME_GC_COLLECT} not declared"));
        self.builder
            .build_call(fn_gc_collect, &[], &format!("call_{NAME_GC_COLLECT}"))
            .unwrap();
    }
}
