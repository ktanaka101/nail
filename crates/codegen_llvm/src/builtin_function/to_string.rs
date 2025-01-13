mod bool_to_string;
mod int_to_string;
mod str_to_string;
mod struct_to_string;

use hir_ty::Monotype;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::Codegen;

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) enum OutputType {
    Int,
    String,
    Boolean,
    // TODO: Add the unit type
    UnitOrStruct,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct Output {
    pub nail_type: OutputType,
    pub value: Value,
}

pub(super) fn define_to_string_functions(codegen: &mut Codegen) {
    int_to_string::define_int_to_string(codegen);
    bool_to_string::define_bool_to_string(codegen);
    str_to_string::define_str_to_string(codegen);
    struct_to_string::define_struct_to_string(codegen);
}

impl<'ctx> Codegen<'_, 'ctx> {
    pub(crate) fn build_call_to_string(
        &self,
        ty: Monotype,
        value: inkwell::values::BasicValueEnum<'ctx>,
    ) -> inkwell::values::PointerValue<'ctx> {
        match ty {
            Monotype::Integer => self.build_call_int_to_string(value.into_int_value()),
            Monotype::Bool => self.build_call_bool_to_string(value.into_int_value()),
            Monotype::String => self.build_call_str_to_string(value.into_pointer_value()),
            Monotype::Struct(_) => self.build_call_struct_to_string(),
            Monotype::Unit => self.build_call_struct_to_string(),
            _ => todo!(),
        }
    }
}
