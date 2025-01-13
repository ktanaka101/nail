use std::ffi::c_void;

#[link(name = "gc")]
extern "C" {
    fn GC_init();
    fn GC_malloc(byte_size: i64) -> *mut c_void;
    fn GC_free(ptr: *mut c_void);
    fn GC_realloc(ptr: *mut c_void, byte_size: i64) -> *mut c_void;
}

/// void GC_init() wrapper
#[no_mangle]
pub(super) extern "C" fn gc_init() {
    unsafe { GC_init() }
}

/// c_void* GC_malloc(i64) wrapper
#[no_mangle]
pub(super) extern "C" fn gc_malloc(byte_size: i64) -> *mut c_void {
    unsafe { GC_malloc(byte_size) }
}

// void GC_free(c_void*) wrapper
#[no_mangle]
pub(super) extern "C" fn gc_free(ptr: *mut c_void) {
    unsafe { GC_free(ptr) }
}

/// c_void* GC_realloc(c_void*, i64) wrapper
#[no_mangle]
pub(super) extern "C" fn gc_realloc(old_ptr: *mut c_void, new_byte_size: i64) -> *mut c_void {
    unsafe { GC_realloc(old_ptr, new_byte_size) }
}
