use llvm_stackmap::{Header, Location, LocationKind, Record, StackMap, StkSizeRecord};
use tracing::level_filters::LevelFilter;
use tracing_subscriber::EnvFilter;

use crate::{
    externs::{nail_gc_collect, nail_gc_init, nail_gc_malloc},
    gc::{GCHeader, GCObjectKind},
};

#[test]
fn test_gc_allocation_and_collection() {
    tracing_subscriber::fmt()
        .with_max_level(LevelFilter::DEBUG)
        .with_env_filter(EnvFilter::new("debug,salsa_2022=off"))
        .init();

    // Initialize GC
    nail_gc_init(1024);

    // Create a simple stackmap for testing
    let stackmap = StackMap {
        header: Header { version: 3 },
        function_records: vec![StkSizeRecord {
            function_address: 0,
            stack_size: 0,
            record_count: 1,
        }],
        constants: vec![],
        records: vec![Record {
            patchpoint_id: 1,
            instruction_offset: 0,
            locations: vec![Location {
                kind: LocationKind::Register,
                size: 8,
                dwarf_reg_num: 0, // Assuming register 0 for test
                offset_or_small_constant: 0,
            }],
            live_outs: vec![],
        }],
    };

    // Set stackmap
    {
        let mut gc = crate::externs::get_gc_lock();
        if let Some(gc) = gc.as_mut() {
            gc.load_stackmap_in_memory_jit(stackmap);
        }
    }

    println!("Stackmap loaded");

    // Allocate some objects (header size = 88 bytes)
    const HEADER_SIZE: usize = std::mem::size_of::<GCHeader>();
    // 16 + 88 = 104. 16bytes alined: 112(16^7)
    let obj1 = nail_gc_malloc(16, GCObjectKind::Struct.to_u8());
    // 32 + 88 = 120. 16 bytes alined: 128(16^8)
    let obj2 = nail_gc_malloc(32, GCObjectKind::Struct.to_u8());
    let obj3 = nail_gc_malloc(1024 - 112 - 128 - HEADER_SIZE, GCObjectKind::Struct.to_u8());

    println!("Objects allocated");

    // Verify allocations
    assert!(!obj1.is_null());
    assert!(!obj2.is_null());
    assert!(!obj3.is_null());

    println!("Objects not null");

    // Run garbage collection
    nail_gc_collect();

    println!("Garbage collection done");

    // Since we don't have actual root references in our test stackmap,
    // objects should be marked for collection.
    // We can verify this indirectly by allocating a new object at offset 0,
    // which should succeed if the heap was cleared
    let new_obj = nail_gc_malloc(1024 - HEADER_SIZE, GCObjectKind::Struct.to_u8());

    println!("New object allocated");

    assert!(!new_obj.is_null());

    println!("New object not null");
}
