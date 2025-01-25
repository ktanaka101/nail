use std::sync::{Mutex, MutexGuard, OnceLock};

use crate::gc::GC;

static mut GLOBAL_GC: OnceLock<Mutex<GC>> = OnceLock::new();

pub(crate) fn get_global_gc() -> MutexGuard<'static, GC> {
    unsafe { GLOBAL_GC.get().unwrap().lock().unwrap() }
}

#[no_mangle]
pub extern "C" fn nail_gc_init() {
    println!("nail_gc_init");

    let gc = Mutex::new(GC::new());
    unsafe { GLOBAL_GC.set(gc).expect("GC already initialized") };
}

#[no_mangle]
pub extern "C" fn nail_gc_shutdown() {
    println!("nail_gc_shutdown");

    if let Some(_gc) = unsafe { GLOBAL_GC.take() } {
        // drop(gc)
    } else {
        println!("GC already shutdown or not initialized");
    }
}

#[no_mangle]
pub extern "C" fn nail_gc_malloc(size: usize) -> *mut u8 {
    println!("nail_gc_malloc: size={}", size);
    let mut gc = get_global_gc();
    gc.gc_alloc(size)
}

#[no_mangle]
pub extern "C" fn nail_gc_collect() {
    println!("nail_gc_collect");

    let mut gc = get_global_gc();
    gc.gc_collect();
}
