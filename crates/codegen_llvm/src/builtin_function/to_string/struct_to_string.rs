use std::ffi::{c_char, CString};

use inkwell::AddressSpace;
use serde_json::Value;

use super::{Output, OutputType};
use crate::Codegen;

#[no_mangle]
extern "C" fn struct_to_string() -> *const c_char {
    let out = Output {
        nail_type: OutputType::UnitOrStruct,
        value: Value::Null,
    };
    let mut json = serde_json::to_string_pretty(&out).unwrap();
    json.push('\n');

    let c = CString::new(json).unwrap();
    c.into_raw()
}

const NAME_STRUCT_TO_STRING: &str = "struct_to_string";
pub(super) fn define_struct_to_string(codegen: &mut Codegen) {
    let return_ty = codegen.context.ptr_type(AddressSpace::default());
    let fn_type = return_ty.fn_type(&[], false);
    let func_value = codegen
        .module
        .add_function(NAME_STRUCT_TO_STRING, fn_type, None);
    codegen
        .execution_engine
        .add_global_mapping(&func_value, struct_to_string as usize);
    codegen
        .builtin_functions
        .insert(NAME_STRUCT_TO_STRING.to_string(), func_value);
}

impl<'ctx> Codegen<'_, 'ctx> {
    pub(crate) fn build_call_struct_to_string(&self) -> inkwell::values::PointerValue<'ctx> {
        let fn_struct_to_string = self
            .module
            .get_function(NAME_STRUCT_TO_STRING)
            .unwrap_or_else(|| panic!("{NAME_STRUCT_TO_STRING} not declared"));
        self.builder
            .build_call(
                fn_struct_to_string,
                &[],
                &format!("call_{NAME_STRUCT_TO_STRING}"),
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value()
    }
}
