use std::ffi::{c_char, CString};

use inkwell::{
    values::{CallSiteValue, FunctionValue, PointerValue},
    AddressSpace,
};
use serde::Serialize;
use serde_json::Value;

use crate::Codegen;

#[derive(Serialize)]
enum OutputType {
    Int,
}

#[derive(Serialize)]
struct Output {
    nail_type: OutputType,
    value: Value,
}

const FN_NAME_PTR_TO_STRING: &str = "ptr_to_string";
#[no_mangle]
extern "C" fn ptr_to_string(ptr: *const i64) -> *const c_char {
    let s = {
        let int = unsafe { *ptr };
        let out = Output {
            nail_type: OutputType::Int,
            value: Value::Number(int.into()),
        };

        let mut json = serde_json::to_string_pretty(&out).unwrap();
        json.push('\n');
        json
    };
    let s = CString::new(s).unwrap();
    s.into_raw()
}
fn define_ptr_to_string(codegen: &mut Codegen) {
    let return_ty = codegen.context.i8_type().ptr_type(AddressSpace::default());
        let fn_type = return_ty.fn_type(
            &[
            codegen
                .context
                    .i64_type()
                    .ptr_type(AddressSpace::default())
                    .into(),
            codegen.context.i64_type().into(),
            ],
            false,
        );
    let func_value = codegen
            .module
            .add_function(FN_NAME_PTR_TO_STRING, fn_type, None);
    codegen
        .execution_engine
            .add_global_mapping(&func_value, ptr_to_string as usize);
    codegen
        .builtin_functions
            .insert(FN_NAME_PTR_TO_STRING.to_string(), func_value);
}

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    pub(super) fn add_builtin_function(&mut self) {
        define_ptr_to_string(self);
    }

    pub(super) fn get_fn_ptr_to_string(&self) -> FunctionValue {
        *self.builtin_functions.get(FN_NAME_PTR_TO_STRING).unwrap()
    }

    pub(super) fn build_call_ptr_to_string(&'a self, ptr: PointerValue<'ctx>) -> CallSiteValue {
        self.builder.build_call(
            self.get_fn_ptr_to_string(),
            &[ptr.into()],
            FN_NAME_PTR_TO_STRING,
        )
    }
}
