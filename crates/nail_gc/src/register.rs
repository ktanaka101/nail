//! Implementation of register access

#[cfg(target_arch = "aarch64")]
mod aarch64;

#[cfg(target_arch = "aarch64")]
pub use aarch64::{get_register_value, Register};

#[cfg(target_arch = "x86_64")]
mod x86_64;

#[cfg(target_arch = "x86_64")]
pub use x86_64::{get_register_value, Register};

#[cfg(not(any(target_arch = "aarch64", target_arch = "x86_64")))]
pub unsafe fn get_register_value(_reg: Register) -> *mut u8 {
    panic!("Register access is only supported on aarch64");
}
