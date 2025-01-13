use std::ffi::{c_char, CString};

use inkwell::AddressSpace;
use serde_json::Value;

use super::{Output, OutputType};
use crate::Codegen;

#[no_mangle]
extern "C" fn str_to_string(ptr: *const u8) -> *const c_char {
    if ptr.is_null() {
        // null pointer is empty string
        let out = Output {
            nail_type: OutputType::String,
            value: Value::String("".to_string()),
        };
        let mut json = serde_json::to_string_pretty(&out).unwrap();
        json.push('\n');
        return CString::new(json).unwrap().into_raw();
    }

    // ptr is a pointer to a null-terminated string.
    let mut bytes = vec![];
    let mut offset = 0usize;
    loop {
        unsafe {
            let b = *ptr.add(offset);
            if b == 0 {
                break;
            }
            bytes.push(b);
        }
        offset += 1;
        // Limit the length of the string to prevent infinite loops.
        if offset > 10_000 {
            break;
        }
    }

    let string = String::from_utf8(bytes).unwrap_or_default();
    let out = Output {
        nail_type: OutputType::String,
        value: Value::String(string),
    };

    let mut json = serde_json::to_string_pretty(&out).unwrap();
    json.push('\n');
    CString::new(json).unwrap().into_raw()
}

const NAME_STR_TO_STRING: &str = "str_to_string";
pub(super) fn define_str_to_string(codegen: &mut Codegen) {
    let return_ty = codegen.context.ptr_type(AddressSpace::default());
    let fn_type = return_ty.fn_type(
        &[codegen.context.ptr_type(AddressSpace::default()).into()],
        false,
    );
    let func_value = codegen
        .module
        .add_function(NAME_STR_TO_STRING, fn_type, None);
    codegen
        .execution_engine
        .add_global_mapping(&func_value, str_to_string as usize);
    codegen
        .builtin_functions
        .insert(NAME_STR_TO_STRING.to_string(), func_value);
}

impl<'ctx> Codegen<'_, 'ctx> {
    pub(crate) fn build_call_str_to_string(
        &self,
        ptr: inkwell::values::PointerValue<'ctx>,
    ) -> inkwell::values::PointerValue<'ctx> {
        let fn_str_to_string = self
            .module
            .get_function(NAME_STR_TO_STRING)
            .unwrap_or_else(|| panic!("{NAME_STR_TO_STRING} not declared"));
        self.builder
            .build_call(
                fn_str_to_string,
                &[ptr.into()],
                &format!("call_{NAME_STR_TO_STRING}"),
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value()
    }
}
