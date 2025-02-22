//! AArch64向けのレジスタアクセス実装

use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum Register {
    X0 = 0,
    X1 = 1,
    X2 = 2,
    X3 = 3,
    X4 = 4,
    X5 = 5,
    X6 = 6,
    X7 = 7,
    X8 = 8,
    X9 = 9,
    X10 = 10,
    X11 = 11,
    X12 = 12,
    X13 = 13,
    X14 = 14,
    X15 = 15,
    X16 = 16,
    X17 = 17,
    X18 = 18,
    X19 = 19,
    X20 = 20,
    X21 = 21,
    X22 = 22,
    X23 = 23,
    X24 = 24,
    X25 = 25,
    X26 = 26,
    X27 = 27,
    X28 = 28,
    X29 = 29, // Frame pointer
    X30 = 30, // Link register
    SP = 31,  // Stack pointer
}

impl Register {
    pub fn from_dwarf_regnum(regnum: u16) -> Option<Self> {
        Self::try_from(regnum as u8).ok()
    }
}

#[cfg(target_arch = "aarch64")]
pub unsafe fn get_register_value(reg: Register) -> *mut u8 {
    let value: u64;
    match reg {
        Register::X0 => {
            std::arch::asm!(
                "mov {}, x0",
                out(reg) value,
            );
        }
        Register::X1 => {
            std::arch::asm!(
                "mov {}, x1",
                out(reg) value,
            );
        }
        Register::X2 => {
            std::arch::asm!(
                "mov {}, x2",
                out(reg) value,
            );
        }
        Register::X3 => {
            std::arch::asm!(
                "mov {}, x3",
                out(reg) value,
            );
        }
        Register::X4 => {
            std::arch::asm!(
                "mov {}, x4",
                out(reg) value,
            );
        }
        Register::X5 => {
            std::arch::asm!(
                "mov {}, x5",
                out(reg) value,
            );
        }
        Register::X6 => {
            std::arch::asm!(
                "mov {}, x6",
                out(reg) value,
            );
        }
        Register::X7 => {
            std::arch::asm!(
                "mov {}, x7",
                out(reg) value,
            );
        }
        Register::X8 => {
            std::arch::asm!(
                "mov {}, x8",
                out(reg) value,
            );
        }
        Register::X9 => {
            std::arch::asm!(
                "mov {}, x9",
                out(reg) value,
            );
        }
        Register::X10 => {
            std::arch::asm!(
                "mov {}, x10",
                out(reg) value,
            );
        }
        Register::X11 => {
            std::arch::asm!(
                "mov {}, x11",
                out(reg) value,
            );
        }
        Register::X12 => {
            std::arch::asm!(
                "mov {}, x12",
                out(reg) value,
            );
        }
        Register::X13 => {
            std::arch::asm!(
                "mov {}, x13",
                out(reg) value,
            );
        }
        Register::X14 => {
            std::arch::asm!(
                "mov {}, x14",
                out(reg) value,
            );
        }
        Register::X15 => {
            std::arch::asm!(
                "mov {}, x15",
                out(reg) value,
            );
        }
        Register::X16 => {
            std::arch::asm!(
                "mov {}, x16",
                out(reg) value,
            );
        }
        Register::X17 => {
            std::arch::asm!(
                "mov {}, x17",
                out(reg) value,
            );
        }
        Register::X18 => {
            std::arch::asm!(
                "mov {}, x18",
                out(reg) value,
            );
        }
        Register::X19 => {
            std::arch::asm!(
                "mov {}, x19",
                out(reg) value,
            );
        }
        Register::X20 => {
            std::arch::asm!(
                "mov {}, x20",
                out(reg) value,
            );
        }
        Register::X21 => {
            std::arch::asm!(
                "mov {}, x21",
                out(reg) value,
            );
        }
        Register::X22 => {
            std::arch::asm!(
                "mov {}, x22",
                out(reg) value,
            );
        }
        Register::X23 => {
            std::arch::asm!(
                "mov {}, x23",
                out(reg) value,
            );
        }
        Register::X24 => {
            std::arch::asm!(
                "mov {}, x24",
                out(reg) value,
            );
        }
        Register::X25 => {
            std::arch::asm!(
                "mov {}, x25",
                out(reg) value,
            );
        }
        Register::X26 => {
            std::arch::asm!(
                "mov {}, x26",
                out(reg) value,
            );
        }
        Register::X27 => {
            std::arch::asm!(
                "mov {}, x27",
                out(reg) value,
            );
        }
        Register::X28 => {
            std::arch::asm!(
                "mov {}, x28",
                out(reg) value,
            );
        }
        Register::X29 => {
            std::arch::asm!(
                "mov {}, x29",
                out(reg) value,
            );
        }
        Register::X30 => {
            std::arch::asm!(
                "mov {}, x30",
                out(reg) value,
            );
        }
        Register::SP => {
            std::arch::asm!(
                "mov {}, sp",
                out(reg) value,
            );
        }
    }
    value as *mut u8
}

#[cfg(not(target_arch = "aarch64"))]
pub unsafe fn get_register_value(_reg: Register) -> *mut u8 {
    panic!("Register access is only supported on aarch64");
}
