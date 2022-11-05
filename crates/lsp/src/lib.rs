mod message;
mod protocol;

use std::io;

use anyhow::Result;

fn server() -> Result<()> {
    let stdin = io::stdin().lock();
    let stdout = io::stdout().lock();

    Ok(())
}

fn main() {
    server().unwrap();
}
