use inkwell::memory_manager::McjitMemoryManager;

#[derive(Debug)]
pub struct MemoryManager {
    fixed_capacity_bytes: usize,
    fixed_page_size: usize,

    code_buff_ptr: std::ptr::NonNull<u8>,
    code_offset: usize,

    data_buff_ptr: std::ptr::NonNull<u8>,
    data_offset: usize,

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
            std::ptr::NonNull::new_unchecked(libc::mmap(
                std::ptr::null_mut(),
                capacity_bytes,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            ) as *mut u8)
        };

        let data_buff_ptr = unsafe {
            std::ptr::NonNull::new_unchecked(libc::mmap(
                std::ptr::null_mut(),
                capacity_bytes,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            ) as *mut u8)
        };

        Self {
            fixed_capacity_bytes: capacity_bytes,
            fixed_page_size: page_size,

            code_buff_ptr,
            code_offset: 0,

            data_buff_ptr,
            data_offset: 0,

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
            libc::mprotect(
                self.code_buff_ptr.as_ptr() as *mut libc::c_void,
                self.fixed_capacity_bytes,
                libc::PROT_READ | libc::PROT_EXEC,
            );
            libc::mprotect(
                self.data_buff_ptr.as_ptr() as *mut libc::c_void,
                self.fixed_capacity_bytes,
                libc::PROT_READ | libc::PROT_WRITE,
            );
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
            libc::munmap(
                self.code_buff_ptr.as_ptr() as *mut libc::c_void,
                self.fixed_capacity_bytes,
            );
            libc::munmap(
                self.data_buff_ptr.as_ptr() as *mut libc::c_void,
                self.fixed_capacity_bytes,
            );
        }
    }
}
