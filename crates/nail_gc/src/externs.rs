use std::sync::{Mutex, MutexGuard};

use crate::gc::{GCObjectKind, GC};

/// Global GC
/// - `None`: Uninitialized
/// - `Some(gc)`: Initialized
static GLOBAL_GC: Mutex<Option<GC>> = Mutex::new(None);

/// ロックを取得し、`Option<GC>` へのミュータブル参照を返す
pub(crate) fn get_gc_lock() -> MutexGuard<'static, Option<GC>> {
    GLOBAL_GC.lock().expect("Mutex poisoned")
}

/// 便利関数: GC が初期化されていればクロージャに &mut GC を渡す。
/// 戻り値: クロージャの戻り値
pub(crate) fn with_global_gc<F, R>(f: F) -> R
where
    F: FnOnce(&mut GC) -> R,
{
    let mut guard = get_gc_lock();
    let gc = guard.as_mut();
    if let Some(gc) = gc {
        f(gc)
    } else {
        panic!("GC not initialized");
    }
}

#[no_mangle]
pub extern "C" fn nail_gc_init(heap_capacity: usize) {
    tracing::debug!("nail_gc_init(heap_capacity={})", heap_capacity);
    let mut guard = get_gc_lock();
    if guard.is_some() {
        panic!("GC already initialized");
    } else {
        *guard = Some(GC::new(heap_capacity));
        tracing::debug!("GC is now initialized");
    }
}

#[no_mangle]
pub extern "C" fn nail_gc_shutdown() {
    tracing::debug!("nail_gc_shutdown");
    let mut guard = get_gc_lock();
    if guard.is_none() {
        panic!("GC not initialized or already shut down");
    } else {
        // dropしてメモリ解放
        *guard = None;
        tracing::debug!("GC was shut down");
    }
}

#[no_mangle]
pub extern "C" fn nail_gc_malloc(size: usize, type_tag: u8) -> *mut u8 {
    let obj_type = GCObjectKind::from_u8(type_tag);
    with_global_gc(|gc| gc.gc_alloc(size, obj_type))
}

#[no_mangle]
pub extern "C" fn nail_gc_collect() {
    with_global_gc(|gc| gc.gc_collect());
}
