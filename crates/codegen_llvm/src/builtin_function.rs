use std::ffi::{c_char, CString};

use inkwell::{
    values::{BasicValueEnum, CallSiteValue, FunctionValue, IntValue, PointerValue},
    AddressSpace,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::Codegen;

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum OutputType {
    Int,
    String,
    Boolean,
    Unit,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Output {
    pub nail_type: OutputType,
    pub value: Value,
}

pub(super) enum PrimitiveType {
    Int,
    String,
    Boolean,
    Unit,
}

impl TryFrom<i64> for PrimitiveType {
    type Error = anyhow::Error;

    fn try_from(value: i64) -> anyhow::Result<Self> {
        Ok(match value {
            1 => PrimitiveType::Int,
            2 => PrimitiveType::String,
            3 => PrimitiveType::Boolean,
            4 => PrimitiveType::Unit,
            other => {
                return Err(anyhow::format_err!(
                    "Expected 1, 2, 3 or 4. Received {other}"
                ))
            }
        })
    }
}
impl From<PrimitiveType> for u64 {
    fn from(val: PrimitiveType) -> Self {
        match val {
            PrimitiveType::Int => 1,
            PrimitiveType::String => 2,
            PrimitiveType::Boolean => 3,
            PrimitiveType::Unit => 4,
        }
    }
}

const FN_NAME_PTR_TO_STRING: &str = "ptr_to_string";
#[no_mangle]
extern "C" fn ptr_to_string(ty: i64, value_ptr: *const i64, _length: i64) -> *const c_char {
    let s = {
        let ty = PrimitiveType::try_from(ty).unwrap();
        let out = match ty {
            PrimitiveType::Int => {
                let int = unsafe { *value_ptr };
                Output {
                    nail_type: OutputType::Int,
                    value: Value::Number(int.into()),
                }
            }
            PrimitiveType::String => {
                let ptr: *const u8 = value_ptr.cast();
                let mut bytes: Vec<u8> = vec![];

                let max = 256;
                for i in 0..max {
                    let v = unsafe { *ptr.add(i) };
                    if v == 0 {
                        break;
                    }
                    bytes.push(v);
                }

                let string = String::from_utf8(bytes).unwrap();
                Output {
                    nail_type: OutputType::String,
                    value: Value::String(string),
                }
            }
            PrimitiveType::Boolean => {
                let value_ptr: *const bool = value_ptr.cast();
                let boolean: bool = unsafe { *value_ptr };
                Output {
                    nail_type: OutputType::Boolean,
                    value: Value::Bool(boolean),
                }
            }
            PrimitiveType::Unit => Output {
                nail_type: OutputType::Unit,
                value: Value::Null,
            },
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
            codegen.context.i64_type().into(),
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

    pub(super) fn get_fn_ptr_to_string(&self) -> FunctionValue<'ctx> {
        *self.builtin_functions.get(FN_NAME_PTR_TO_STRING).unwrap()
    }

    pub(super) fn build_call_unit_string(&'a self) -> CallSiteValue<'ctx> {
        self.build_call_ptr_to_string(
            PrimitiveType::Unit,
            self.context
                .i64_type()
                .ptr_type(AddressSpace::default())
                .const_zero(),
            self.context.i64_type().const_zero(),
        )
    }

    pub(super) fn build_to_string(&'a self, value: BasicValueEnum<'ctx>) -> CallSiteValue<'ctx> {
        match value {
            BasicValueEnum::IntValue(int) => {
                let value_ptr = self.builder.build_alloca(int.get_type(), "alloca_value");
                self.builder.build_store(value_ptr, value);

                if int.get_type().get_bit_width() == 1 {
                    self.build_call_ptr_to_string(
                        PrimitiveType::Boolean,
                        value_ptr,
                        self.context.i64_type().const_zero(),
                    )
                } else {
                    self.build_call_ptr_to_string(
                        PrimitiveType::Int,
                        value_ptr,
                        self.context.i64_type().const_zero(),
                    )
                }
            }
            BasicValueEnum::PointerValue(ptr) => self.build_call_ptr_to_string(
                PrimitiveType::String,
                ptr,
                self.context.i64_type().const_zero(),
            ),
            BasicValueEnum::StructValue(_) => self.build_call_unit_string(),
            _ => unimplemented!(),
        }
    }

    pub(super) fn build_call_ptr_to_string(
        &'a self,
        ty: PrimitiveType,
        value_ptr: PointerValue<'ctx>,
        length: IntValue<'ctx>,
    ) -> CallSiteValue<'ctx> {
        self.builder.build_call(
            self.get_fn_ptr_to_string(),
            &[
                self.context.i64_type().const_int(ty.into(), false).into(),
                value_ptr.into(),
                length.into(),
            ],
            FN_NAME_PTR_TO_STRING,
        )
    }
}
