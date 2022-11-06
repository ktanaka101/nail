pub mod message;
pub mod protocol;

use std::io;

use anyhow::Result;

pub fn server() -> Result<()> {
    let _stdin = io::stdin().lock();
    let _stdout = io::stdout().lock();

    Ok(())
}
