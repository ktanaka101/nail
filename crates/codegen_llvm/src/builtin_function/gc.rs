mod gc_collect;
mod gc_malloc;

use gc_collect::define_gc_collect;
use gc_malloc::define_gc_malloc;

use crate::Codegen;

pub(super) fn define_gc_functions(codegen: &mut Codegen) {
    define_gc_collect(codegen);
    define_gc_malloc(codegen);
}
