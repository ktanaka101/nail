use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum Register {
    /// RAX
    Rax = 0,
    /// RDX
    Rdx = 1,
    /// RCX
    Rcx = 2,
    /// RBX
    Rbx = 3,
    /// RSI
    Rsi = 4,
    /// RDI
    Rdi = 5,
    /// RBP (frame pointer)
    Rbp = 6,
    /// RSP (stack pointer)
    Rsp = 7,
    /// R8
    R8 = 8,
    /// R9
    R9 = 9,
    /// R10
    R10 = 10,
    /// R11
    R11 = 11,
    /// R12
    R12 = 12,
    /// R13
    R13 = 13,
    /// R14
    R14 = 14,
    /// R15
    R15 = 15,
    // (Define additional registers such as RIP or flags if necessary.)
}

impl Register {
    /// Intended for converting from a DWARF register number, etc.
    pub fn from_dwarf_regnum(regnum: u16) -> Option<Self> {
        Self::try_from(regnum as u8).ok()
    }
}

///
/// # Safety
///
/// - This function uses inline assembly to retrieve the contents of the specified register as a `u64`,
///   stores it in a local variable, and returns its address.
/// - Because the returned pointer will point to the stack area after the function returns, it may
///   effectively be an invalid pointer, and using it in the caller could lead to undefined behavior.
///
pub unsafe fn get_register_value(reg: Register) -> *mut u8 {
    let value: u64;
    match reg {
        Register::Rax => {
            std::arch::asm!(
                "mov {}, rax",
                out(reg) value,
            );
        }
        Register::Rdx => {
            std::arch::asm!(
                "mov {}, rdx",
                out(reg) value,
            );
        }
        Register::Rcx => {
            std::arch::asm!(
                "mov {}, rcx",
                out(reg) value,
            );
        }
        Register::Rbx => {
            std::arch::asm!(
                "mov {}, rbx",
                out(reg) value,
            );
        }
        Register::Rsi => {
            std::arch::asm!(
                "mov {}, rsi",
                out(reg) value,
            );
        }
        Register::Rdi => {
            std::arch::asm!(
                "mov {}, rdi",
                out(reg) value,
            );
        }
        Register::Rbp => {
            std::arch::asm!(
                "mov {}, rbp",
                out(reg) value,
            );
        }
        Register::Rsp => {
            std::arch::asm!(
                "mov {}, rsp",
                out(reg) value,
            );
        }
        Register::R8 => {
            std::arch::asm!(
                "mov {}, r8",
                out(reg) value,
            );
        }
        Register::R9 => {
            std::arch::asm!(
                "mov {}, r9",
                out(reg) value,
            );
        }
        Register::R10 => {
            std::arch::asm!(
                "mov {}, r10",
                out(reg) value,
            );
        }
        Register::R11 => {
            std::arch::asm!(
                "mov {}, r11",
                out(reg) value,
            );
        }
        Register::R12 => {
            std::arch::asm!(
                "mov {}, r12",
                out(reg) value,
            );
        }
        Register::R13 => {
            std::arch::asm!(
                "mov {}, r13",
                out(reg) value,
            );
        }
        Register::R14 => {
            std::arch::asm!(
                "mov {}, r14",
                out(reg) value,
            );
        }
        Register::R15 => {
            std::arch::asm!(
                "mov {}, r15",
                out(reg) value,
            );
        }
    }

    // In reality, this is the address of the local variable `value`, so it ceases to exist after the function returns.
    value as *mut u8
}
