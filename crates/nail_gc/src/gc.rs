use std::ptr::{self, null_mut};

// 簡易的なオブジェクトヘッダなど
#[derive(Debug)]
pub(crate) struct GCObject {
    pub(crate) alive_mark: bool,
    pub(crate) size: usize,
    // payload starts here
}

#[derive(Debug)]
pub(crate) struct GC {
    // JIT stackmap data pointer
    stackmap_ptr: Option<ptr::NonNull<u8>>,
    stackmap_size: usize,

    // A simple bump pointer region for GC objects (toy)
    heap: Box<[u8]>,
    heap_offset: usize,

    finalized: bool,
    destroyed: bool,

    // a naive list of allocated objects
    // store the ptr to the object header on heap
    allocated_objs: Vec<ptr::NonNull<GCObject>>,
}
unsafe impl Send for GC {}

impl GC {
    pub(crate) fn new() -> Self {
        // 1) allocate a 1KiB region for the GC-managed heap
        let heap_capacity = 1024;
        let heap = vec![0u8; heap_capacity];
        let heap = heap.into_boxed_slice();

        Self {
            stackmap_ptr: None,
            stackmap_size: 0,
            heap,
            heap_offset: 0,
            finalized: false,
            destroyed: false,
            allocated_objs: vec![],
        }
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.heap.len()
    }

    pub(crate) fn gc_alloc(&mut self, size: usize) -> *mut u8 {
        // very naive bump pointer
        let align = std::mem::align_of::<GCObject>().max(16);
        let offset_aligned = (self.heap_offset + align - 1) & !(align - 1);
        let total = offset_aligned + std::mem::size_of::<GCObject>() + size;
        if total > self.capacity() {
            println!("Out of GC heap => force collect?");
            // self.gc_collect();
        }

        // create object header
        let obj_ptr = unsafe { self.heap.as_mut_ptr().add(offset_aligned) as *mut GCObject };
        unsafe {
            // init
            (*obj_ptr).alive_mark = false;
            (*obj_ptr).size = size;
        }

        self.heap_offset = total;

        let nonnull_obj_ptr = ptr::NonNull::new(obj_ptr).expect("obj_ptr shouldn't be null");
        self.allocated_objs.push(nonnull_obj_ptr);

        unsafe { obj_ptr.add(1) as *mut u8 }
    }

    pub(crate) fn gc_collect(&mut self) {
        println!("---- GC Collect start (Mark-Sweep) ----");

        // 1) Mark phase: we would read from the stack (or from stackmap records) to find root regs
        // For demonstration: we'll just pretend all are "dead" except the first
        if let Some(first) = self.allocated_objs.first() {
            unsafe {
                (*first.as_ptr()).alive_mark = true;
            }
            println!("Mark first object as alive => demonstration");
        }

        // 2) Sweep phase
        let mut new_objs = Vec::new();
        for &obj_ptr in &self.allocated_objs {
            let alive = unsafe { obj_ptr.as_ref().alive_mark };
            if alive {
                // keep it, clear mark
                unsafe {
                    let mut_ref = obj_ptr.as_ptr();
                    (*mut_ref).alive_mark = false;
                }
                new_objs.push(obj_ptr);
            } else {
                // drop it (do nothing for now)
                println!("Drop object => {:?}", obj_ptr);
            }
        }
        self.allocated_objs = new_objs;

        println!("---- GC Collect end ----");
    }
}

/// for JIT
///
/// (1) load_stackmap_in_memory_jit: called by JIT to load the stackmap data in memory
impl GC {
    pub(crate) fn load_stackmap_in_memory_jit(&mut self, buf: *mut u8, size: usize) {
        println!(
            "Load stackmap data => ptr={:?}, size={}",
            self.stackmap_ptr, self.stackmap_size
        );
        self.stackmap_ptr = Some(ptr::NonNull::new(buf).expect("stackmap must be not null ptr"));
        self.stackmap_size = size;

        if self.stackmap_size == 0 {
            println!("No .llvm_stackmaps data => skipping parse");
        }

        // TODO: parse the stackmap data
        // Pseudo parse example
        // In reality, you'd interpret the header, records, etc.
        // store them in some self.stackmapsRecords ...
    }
}
