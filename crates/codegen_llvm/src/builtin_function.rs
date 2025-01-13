pub mod to_string;

use to_string::define_to_string_functions;

use crate::Codegen;

impl Codegen<'_, '_> {
    pub(super) fn add_builtin_function(&mut self) {
        define_to_string_functions(self);
    }
}
