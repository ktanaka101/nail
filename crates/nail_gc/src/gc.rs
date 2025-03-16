use std::{ptr, sync::Arc};

use llvm_stackmap::StackMap;

use crate::register::{get_register_value, Register};

// Object type
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GCObjectKind {
    Struct,
}

impl GCObjectKind {
    pub fn to_u8(self) -> u8 {
        match self {
            Self::Struct => 0,
        }
    }

    pub fn from_u8(value: u8) -> Self {
        match value {
            0 => Self::Struct,
            _ => panic!("Invalid GC object type tag: {}", value),
        }
    }
}

/// Field offset information used by GC when following references.
#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct FieldOffset {
    pub(crate) offset: usize,
    pub(crate) is_ptr: bool,
}

/// Type descriptor for GC objects
#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct TypeDescriptor {
    pub(crate) kind: GCObjectKind,
    pub(crate) type_name: &'static str,
    pub(crate) size_of: usize,
    pub(crate) fields: Vec<FieldOffset>,
}

// GC object header
#[derive(Debug)]
pub(crate) struct GCHeader {
    pub(crate) alive_mark: bool,
    pub(crate) size: usize,
    pub(crate) type_info: TypeDescriptor,
    /// Track references to other objects
    pub(crate) references: Vec<ptr::NonNull<GCHeader>>,
}

#[derive(Debug)]
pub(crate) struct GC {
    stackmap: Option<Arc<StackMap>>,

    // A simple bump pointer region for GC objects
    heap: Box<[u8]>,
    heap_offset: usize,

    // List of allocated objects
    allocated_objs: Vec<ptr::NonNull<GCHeader>>,
}

unsafe impl Send for GC {}

impl GC {
    pub(crate) fn new(heap_capacity: usize) -> Self {
        let heap = vec![0u8; heap_capacity];
        let heap = heap.into_boxed_slice();

        Self {
            stackmap: None,
            heap,
            heap_offset: 0,
            allocated_objs: vec![],
        }
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.heap.len()
    }

    // Determine if a pointer is within the heap
    fn is_heap_ptr(&self, ptr: *mut u8) -> bool {
        let heap_start = self.heap.as_ptr() as usize;
        let heap_end = heap_start + self.heap.len();
        let ptr_addr = ptr as usize;
        ptr_addr >= heap_start && ptr_addr < heap_end
    }

    // Collect references based on object type
    fn collect_references(&self, _obj: &GCHeader) -> Vec<ptr::NonNull<GCHeader>> {
        // TODO: Implement field offset calculation
        Vec::new()
    }

    pub(crate) fn gc_alloc(&mut self, size: usize, obj_type: GCObjectKind) -> *mut u8 {
        // Check if we need to collect garbage
        let align = std::mem::align_of::<GCHeader>().max(16);
        let offset_aligned = (self.heap_offset + align - 1) & !(align - 1);
        let total = offset_aligned + std::mem::size_of::<GCHeader>() + size;
        tracing::debug!(
            "Allocating object: size={}, type={:?}, total={} offset_aligned={}. GCHeader aligned={}",
            size,
            obj_type,
            total,
            offset_aligned,
            align
        );

        if total > self.capacity() {
            tracing::debug!("Out of GC heap => force collect");
            self.gc_collect();

            // After collection, check if we have enough space
            if total > self.capacity() {
                panic!("Out of memory even after GC");
            }
        }

        // create object header
        let obj_ptr = unsafe { self.heap.as_mut_ptr().add(offset_aligned) as *mut GCHeader };
        unsafe {
            (*obj_ptr).alive_mark = false;
            (*obj_ptr).size = size;
            // TODO: Set type info
            (*obj_ptr).type_info = TypeDescriptor {
                kind: obj_type,
                type_name: "Unknown",
                size_of: size,
                fields: Vec::new(),
            };
            (*obj_ptr).references = Vec::new();
        }

        self.heap_offset = total;

        let nonnull_obj_ptr = ptr::NonNull::new(obj_ptr).expect("obj_ptr shouldn't be null");
        self.allocated_objs.push(nonnull_obj_ptr);

        unsafe { obj_ptr.add(1) as *mut u8 }
    }

    fn mark_object_and_get_refs(
        &mut self,
        obj_ptr: ptr::NonNull<GCHeader>,
    ) -> Vec<ptr::NonNull<GCHeader>> {
        unsafe {
            let obj_ref = obj_ptr.as_ptr();
            if (*obj_ref).alive_mark {
                return Vec::new(); // Already marked
            }

            // Mark this object
            (*obj_ref).alive_mark = true;

            // Collect references
            let refs = self.collect_references(&*obj_ref);
            (*obj_ref).references = refs.clone();
            refs
        }
    }

    fn mark_object(&mut self, obj_ptr: ptr::NonNull<GCHeader>) {
        let mut to_mark = vec![obj_ptr];
        while let Some(current) = to_mark.pop() {
            let refs = self.mark_object_and_get_refs(current);
            to_mark.extend(refs);
        }
    }

    fn mark_from_location(&mut self, location: &llvm_stackmap::Location) {
        tracing::debug!("Marking from location: {:?}", location);

        // Get pointer from stackmap location
        let ptr = match location.kind {
            llvm_stackmap::LocationKind::Register => {
                // Get pointer from register
                if let Some(reg) = Register::from_dwarf_regnum(location.dwarf_reg_num) {
                    unsafe { get_register_value(reg) }
                } else {
                    tracing::warn!("Unknown register number: {}", location.dwarf_reg_num);
                    return;
                }
            }
            llvm_stackmap::LocationKind::Direct => {
                // Get pointer from register + offset
                if let Some(reg) = Register::from_dwarf_regnum(location.dwarf_reg_num) {
                    let base_ptr = unsafe { get_register_value(reg) };
                    unsafe { base_ptr.offset(location.offset_or_small_constant as isize) }
                } else {
                    tracing::warn!("Unknown register number: {}", location.dwarf_reg_num);
                    return;
                }
            }
            llvm_stackmap::LocationKind::Indirect => {
                // Get pointer from indirect reference
                if let Some(reg) = Register::from_dwarf_regnum(location.dwarf_reg_num) {
                    let base_ptr = unsafe { get_register_value(reg) };
                    let ptr_ptr =
                        unsafe { base_ptr.offset(location.offset_or_small_constant as isize) };
                    unsafe { *(ptr_ptr as *mut *mut u8) }
                } else {
                    tracing::warn!("Unknown register number: {}", location.dwarf_reg_num);
                    return;
                }
            }
            llvm_stackmap::LocationKind::Constant => return, // Constants are not currently supported
            llvm_stackmap::LocationKind::ConstantIndex => return, // Constant indices are not currently supported
        };

        // Check if pointer points within the heap
        if !self.is_heap_ptr(ptr) {
            return;
        }

        // Mark as GC object
        let obj_ptr = ptr as *mut GCHeader;
        if let Some(nonnull) = ptr::NonNull::new(obj_ptr) {
            self.mark_object(nonnull);
        }
    }

    pub(crate) fn gc_collect(&mut self) {
        tracing::debug!("---- GC Collect start (Mark-Sweep) ----");

        // Reset all marks
        for &obj_ptr in &self.allocated_objs {
            unsafe {
                (*obj_ptr.as_ptr()).alive_mark = false;
            }
        }

        // Collect roots from stackmap
        let stackmap = self.stackmap.clone();
        let locations: Vec<_> = if let Some(stackmap) = &stackmap {
            stackmap
                .records
                .iter()
                .flat_map(|record| record.locations.iter())
                .collect()
        } else {
            tracing::warn!("No stackmap available for GC");
            Vec::new()
        };

        // Execute marking process from collected locations
        if !locations.is_empty() {
            for location in locations {
                self.mark_from_location(location);
            }
        } else {
            // If no stackmap exists, conservatively mark all objects as alive
            // Length won't change by mark_object.
            // However, malloc must not be called asynchronously.
            for i in 0..self.allocated_objs.len() {
                self.mark_object(self.allocated_objs[i]);
            }
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
        self.stackmap = Some(Arc::new(stackmap));
        tracing::debug!("Loaded stackmap in memory JIT");
    }
}
