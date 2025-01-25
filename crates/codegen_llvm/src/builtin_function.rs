pub mod gc;
pub mod stackmap;
pub mod to_string;

use gc::define_gc_functions;
use stackmap::define_stackmap_functions;
use to_string::define_to_string_functions;

use crate::Codegen;

impl Codegen<'_, '_> {
    pub(super) fn add_builtin_function(&mut self) {
        define_stackmap_functions(self);
        define_gc_functions(self);
        define_to_string_functions(self);
    }
}
