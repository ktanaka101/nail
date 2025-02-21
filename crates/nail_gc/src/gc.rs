use std::ptr;

use llvm_stackmap::StackMap;

// オブジェクトタイプ
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum GCObjectType {
    Integer,
    Float,
    String,
    Array,
    Object,
}

// GCオブジェクトヘッダ
#[derive(Debug)]
pub(crate) struct GCObject {
    pub(crate) alive_mark: bool,
    pub(crate) size: usize,
    pub(crate) obj_type: GCObjectType,
    pub(crate) references: Vec<ptr::NonNull<GCObject>>, // 他のオブジェクトへの参照を追跡
}

#[derive(Debug)]
pub(crate) struct GC {
    stackmap: Option<StackMap>,

    // A simple bump pointer region for GC objects
    heap: Box<[u8]>,
    heap_offset: usize,

    finalized: bool,
    destroyed: bool,

    // List of allocated objects
    allocated_objs: Vec<ptr::NonNull<GCObject>>,
}

unsafe impl Send for GC {}

impl GC {
    pub(crate) fn new() -> Self {
        let heap_capacity = 1024 * 1024; // 1MB
        let heap = vec![0u8; heap_capacity];
        let heap = heap.into_boxed_slice();

        Self {
            stackmap: None,
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

    // ヒープ内のポインタかどうかを判定
    fn is_heap_ptr(&self, ptr: *mut u8) -> bool {
        let heap_start = self.heap.as_ptr() as usize;
        let heap_end = heap_start + self.heap.len();
        let ptr_addr = ptr as usize;
        ptr_addr >= heap_start && ptr_addr < heap_end
    }

    // オブジェクトタイプに基づいて参照を収集
    fn collect_references(&self, obj: &GCObject) -> Vec<ptr::NonNull<GCObject>> {
        let mut refs = Vec::new();

        // オブジェクトタイプに応じて参照を収集
        match obj.obj_type {
            GCObjectType::Array | GCObjectType::Object => {
                // ペイロードをポインタの配列として解釈
                let payload_ptr = unsafe { (obj as *const GCObject).add(1) as *const *mut u8 };
                let payload_len = obj.size / std::mem::size_of::<*mut u8>();

                for i in 0..payload_len {
                    let ptr = unsafe { *payload_ptr.add(i) };
                    if self.is_heap_ptr(ptr) {
                        let obj_ptr = ptr as *mut GCObject;
                        if let Some(nonnull) = ptr::NonNull::new(obj_ptr) {
                            refs.push(nonnull);
                        }
                    }
                }
            }
            _ => {} // 他のタイプは参照を持たない
        }

        refs
    }

    pub(crate) fn gc_alloc(&mut self, size: usize, obj_type: GCObjectType) -> *mut u8 {
        // Check if we need to collect garbage
        let align = std::mem::align_of::<GCObject>().max(16);
        let offset_aligned = (self.heap_offset + align - 1) & !(align - 1);
        let total = offset_aligned + std::mem::size_of::<GCObject>() + size;

        if total > self.capacity() {
            tracing::debug!("Out of GC heap => force collect");
            self.gc_collect();

            // After collection, check if we have enough space
            if total > self.capacity() {
                panic!("Out of memory even after GC");
            }
        }

        // create object header
        let obj_ptr = unsafe { self.heap.as_mut_ptr().add(offset_aligned) as *mut GCObject };
        unsafe {
            // init
            (*obj_ptr).alive_mark = false;
            (*obj_ptr).size = size;
            (*obj_ptr).obj_type = obj_type;
            (*obj_ptr).references = Vec::new();
        }

        self.heap_offset = total;

        let nonnull_obj_ptr = ptr::NonNull::new(obj_ptr).expect("obj_ptr shouldn't be null");
        self.allocated_objs.push(nonnull_obj_ptr);

        unsafe { obj_ptr.add(1) as *mut u8 }
    }

    fn mark_object(&mut self, obj_ptr: ptr::NonNull<GCObject>) {
        unsafe {
            let obj_ref = obj_ptr.as_ptr();
            if (*obj_ref).alive_mark {
                return; // Already marked
            }

            // Mark this object
            (*obj_ref).alive_mark = true;

            // Update references
            (*obj_ref).references = self.collect_references(&*obj_ref);

            // Recursively mark all referenced objects
            let refs = (*obj_ref).references.clone();
            for &ref_ptr in &refs {
                self.mark_object(ref_ptr);
            }
        }
    }

    fn mark_from_location(&mut self, offset: usize) {
        // 実装予定: スタックマップデータからルートを特定
        tracing::debug!("Marking from location at offset {}", offset);
    }

    pub(crate) fn gc_collect(&mut self) {
        tracing::debug!("---- GC Collect start (Mark-Sweep) ----");

        // Reset all marks
        for &obj_ptr in &self.allocated_objs {
            unsafe {
                (*obj_ptr.as_ptr()).alive_mark = false;
            }
        }

        // Additional mark phase for global roots (if any)
        if let Some(first) = self.allocated_objs.first() {
            self.mark_object(*first);
        }

        // Sweep phase
        let mut new_objs = Vec::new();
        for &obj_ptr in &self.allocated_objs {
            let alive = unsafe { obj_ptr.as_ref().alive_mark };
            if alive {
                new_objs.push(obj_ptr);
            } else {
                tracing::debug!("Drop object => {:?}", obj_ptr);
            }
        }

        // Update allocated objects list
        self.allocated_objs = new_objs;

        // Reset heap offset if all objects are dead
        if self.allocated_objs.is_empty() {
            self.heap_offset = 0;
        }

        tracing::debug!("---- GC Collect end ----");
    }
}

/// for JIT
impl GC {
    pub(crate) fn load_stackmap_in_memory_jit(&mut self, stackmap: StackMap) {
        self.stackmap = Some(stackmap);
        tracing::debug!("Loaded stackmap in memory JIT");
    }
}
