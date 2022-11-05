mod message;
mod protocol;

use std::io;

use anyhow::Result;

pub fn server() -> Result<()> {
    let stdin = io::stdin().lock();
    let stdout = io::stdout().lock();

    Ok(())
}
