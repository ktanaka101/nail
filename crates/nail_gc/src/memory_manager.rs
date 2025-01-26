use std::ptr::null_mut;

use inkwell::memory_manager::McjitMemoryManager;
use libc::{mprotect, PROT_EXEC, PROT_READ, PROT_WRITE};

use crate::externs::with_global_gc;

#[derive(Debug)]
pub struct MarkSweepMemoryManager {
    code_buf_ptr: *mut libc::c_void,
    code_buf_size: usize,

    data_buf_ptr: *mut libc::c_void,
    data_buf_size: usize,

    finalized: bool,
    destroyed: bool,
}

impl MarkSweepMemoryManager {
    fn new() -> Self {
        Self {
            code_buf_ptr: null_mut(),
            code_buf_size: 0,
            data_buf_ptr: null_mut(),
            data_buf_size: 0,
            finalized: false,
            destroyed: false,
        }
    }
}

// -------------------- JIT Memory Manager -------------------------
impl McjitMemoryManager for MarkSweepMemoryManager {
    fn allocate_code_section(
        &mut self,
        size: libc::uintptr_t,
        alignment: libc::c_uint,
        section_id: libc::c_uint,
        section_name: &str,
    ) -> *mut u8 {
        tracing::debug!("allocate_code_section: section_id={section_id} section_name={section_name} size={size} alignment={alignment}");

        let code_buf_ptr = unsafe { libc::malloc(size) };
        if code_buf_ptr.is_null() {
            tracing::debug!("Failed to allocate code section");
            return null_mut();
        }

        self.code_buf_ptr = code_buf_ptr;
        self.code_buf_size = size;

        code_buf_ptr as *mut u8
    }

    fn allocate_data_section(
        &mut self,
        size: libc::uintptr_t,
        alignment: libc::c_uint,
        section_id: libc::c_uint,
        section_name: &str,
        is_read_only: bool,
    ) -> *mut u8 {
        tracing::debug!("allocate_data_section: section_id={section_id} section_name={section_name} size={size} alignment={alignment} readonly={is_read_only}");

        let data_buf_ptr = unsafe { libc::malloc(size) };
        if data_buf_ptr.is_null() {
            tracing::debug!("Failed to allocate data section");
            return null_mut();
        }

        self.data_buf_ptr = data_buf_ptr;
        self.data_buf_size = size;

        data_buf_ptr as *mut u8
    }

    fn finalize_memory(&mut self) -> Result<(), String> {
        tracing::debug!("MarkSweepMemoryManager finalize_memory");
        if self.finalized {
            panic!("MemoryManager already finalized");
        }
        self.finalized = true;

        // mprotect code region to RX
        self.mprotect_rx(self.code_buf_ptr, self.code_buf_size)?;

        // mprotect data region to RW
        self.mprotect_rw(self.data_buf_ptr, self.data_buf_size)?;

        with_global_gc(|gc| {
            gc.load_stackmap_in_memory_jit(self.data_buf_ptr as *mut u8, self.data_buf_size);
        });

        Ok(())
    }

    fn destroy(&mut self) {
        tracing::debug!("MarkSweepMemoryManager destroy");
        if self.destroyed {
            panic!("MemoryManager already destroyed");
        }
        self.destroyed = true;

        // free stackmap
        if !self.code_buf_ptr.is_null() {
            unsafe {
                libc::free(self.code_buf_ptr);
            }
            self.code_buf_ptr = null_mut();
            self.code_buf_size = 0;
        }

        // free heap
        if !self.data_buf_ptr.is_null() {
            unsafe {
                libc::free(self.data_buf_ptr);
            }
            self.data_buf_ptr = null_mut();
            self.data_buf_size = 0;
        }
    }
}

impl MarkSweepMemoryManager {
    // -- private utilities
    fn mprotect_rx(&self, ptr: *mut libc::c_void, size: usize) -> Result<(), String> {
        let page_size = 4096;
        let size_rounded = (size + page_size - 1) & !(page_size - 1);
        let ret = unsafe { mprotect(ptr, size_rounded, PROT_READ | PROT_EXEC) };
        if ret != 0 {
            return Err("mprotect code buf to RX failed".into());
        }
        Ok(())
    }

    fn mprotect_rw(&self, ptr: *mut libc::c_void, size: usize) -> Result<(), String> {
        let page_size = 4096;
        let size_rounded = (size + page_size - 1) & !(page_size - 1);
        let ret = unsafe { mprotect(ptr, size_rounded, PROT_READ | PROT_WRITE) };
        if ret != 0 {
            return Err("mprotect data buf to RW failed".into());
        }
        Ok(())
    }
}
