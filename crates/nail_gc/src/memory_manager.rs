use core::slice;

use inkwell::memory_manager::McjitMemoryManager;
use llvm_stackmap::StackMap;

use crate::externs::{get_gc_lock, with_global_gc};

#[derive(Debug)]
pub struct MemoryManager {
    fixed_capacity_bytes: usize,
    fixed_page_size: usize,

    code_buff_ptr: std::ptr::NonNull<u8>,
    code_offset: usize,

    data_buff_ptr: std::ptr::NonNull<u8>,
    data_offset: usize,

    stack_map_ptr: std::ptr::NonNull<u8>,
    stack_map_size: usize,

    finalized: bool,
    destroyed: bool,
}

impl Default for MemoryManager {
    fn default() -> Self {
        Self::new()
    }
}

impl MemoryManager {
    pub fn new() -> Self {
        let capacity_bytes = 128 * 1024;
        let page_size = unsafe { libc::sysconf(libc::_SC_PAGESIZE) as usize };

        let code_buff_ptr = unsafe {
            std::ptr::NonNull::new(libc::mmap(
                std::ptr::null_mut(),
                capacity_bytes,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            ) as *mut u8)
            .expect("code buff mmap failed")
        };

        let data_buff_ptr = unsafe {
            std::ptr::NonNull::new(libc::mmap(
                std::ptr::null_mut(),
                capacity_bytes,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            ) as *mut u8)
            .expect("data buff mmap failed")
        };

        let stackmap_buff_ptr = unsafe {
            std::ptr::NonNull::new(libc::mmap(
                std::ptr::null_mut(),
                capacity_bytes,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            ) as *mut u8)
            .expect("stackmap buff mmap failed")
        };

        Self {
            fixed_capacity_bytes: capacity_bytes,
            fixed_page_size: page_size,

            code_buff_ptr,
            code_offset: 0,

            data_buff_ptr,
            data_offset: 0,

            stack_map_ptr: stackmap_buff_ptr,
            stack_map_size: 0,

            finalized: false,
            destroyed: false,
        }
    }
}

impl McjitMemoryManager for MemoryManager {
    fn allocate_code_section(
        &mut self,
        size: libc::size_t,
        alignment: libc::c_uint,
        section_id: libc::c_uint,
        section_name: &str,
    ) -> *mut u8 {
        tracing::debug!("allocate_code_section: section_id={section_id} section_name={section_name} size={size} alignment={alignment}");

        let alloc_size = size.div_ceil(self.fixed_page_size) * self.fixed_page_size;
        let ptr = unsafe { self.code_buff_ptr.as_ptr().add(self.code_offset) };
        self.code_offset += alloc_size;

        ptr
    }

    fn allocate_data_section(
        &mut self,
        size: libc::size_t,
        alignment: libc::c_uint,
        section_id: libc::c_uint,
        section_name: &str,
        is_read_only: bool,
    ) -> *mut u8 {
        tracing::debug!("allocate_data_section: section_id={section_id} section_name={section_name} size={size} alignment={alignment} readonly={is_read_only}");

        if section_name == ".llvm_stackmaps" {
            self.stack_map_size = size;
            return self.stack_map_ptr.as_ptr();
        }

        let alloc_size = size.div_ceil(self.fixed_page_size) * self.fixed_page_size;
        let ptr = unsafe { self.data_buff_ptr.as_ptr().add(self.data_offset) };
        self.data_offset += alloc_size;

        ptr
    }

    fn finalize_memory(&mut self) -> Result<(), String> {
        tracing::debug!("finalize_memory");

        if self.finalized {
            panic!("MemoryManager already finalized");
        }
        self.finalized = true;

        unsafe {
            let result = libc::mprotect(
                self.code_buff_ptr.as_ptr() as *mut libc::c_void,
                self.fixed_capacity_bytes,
                libc::PROT_READ | libc::PROT_EXEC,
            );
            if result != 0 {
                tracing::warn!("mprotect code_buff failed");
                return Err("mprotect code_buff failed".to_string());
            }

            libc::mprotect(
                self.data_buff_ptr.as_ptr() as *mut libc::c_void,
                self.fixed_capacity_bytes,
                libc::PROT_READ | libc::PROT_WRITE,
            );
            if result != 0 {
                tracing::warn!("mprotect data_buff failed");
                return Err("mprotect data_buff failed".to_string());
            }

            libc::mprotect(
                self.stack_map_ptr.as_ptr() as *mut libc::c_void,
                self.fixed_capacity_bytes,
                libc::PROT_READ,
            );
            if result != 0 {
                tracing::warn!("mprotect stackmap_buff failed");
                return Err("mprotect stackmap_buff failed".to_string());
            }
        }

        let mut binding = get_gc_lock();
        let gc = binding.as_mut();
        if let Some(gc) = gc {
            let stackmap_data =
                unsafe { slice::from_raw_parts(self.stack_map_ptr.as_ptr(), self.stack_map_size) };
            let stackmap = StackMap::parse(stackmap_data.to_vec()).unwrap();
            gc.load_stackmap_in_memory_jit(stackmap);
        } else {
            tracing::warn!("GC is not initialized");
        }

        Ok(())
    }

    fn destroy(&mut self) {
        tracing::debug!("destroy");

        if self.destroyed {
            panic!("MemoryManager already destroyed");
        }
        self.destroyed = true;

        unsafe {
            let result = libc::munmap(
                self.code_buff_ptr.as_ptr() as *mut libc::c_void,
                self.fixed_capacity_bytes,
            );
            if result != 0 {
                tracing::error!("munmap code_buff failed");
                panic!("munmap code_buff failed");
            }

            let result = libc::munmap(
                self.data_buff_ptr.as_ptr() as *mut libc::c_void,
                self.fixed_capacity_bytes,
            );
            if result != 0 {
                tracing::error!("munmap data_buff failed");
                panic!("munmap data_buff failed");
            }

            let result = libc::munmap(
                self.stack_map_ptr.as_ptr() as *mut libc::c_void,
                self.fixed_capacity_bytes,
            );
            if result != 0 {
                tracing::error!("munmap stackmap_buff failed");
                panic!("munmap stackmap_buff failed");
            }
        }
    }
}
