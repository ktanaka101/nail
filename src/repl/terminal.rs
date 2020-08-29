use std::convert::TryInto;
use std::io;
use std::io::Write;

use termion;

pub struct Terminal {
    row: u16,
    col: u16,
    stdout: termion::raw::RawTerminal<io::Stdout>,
    last_write_col: u16,
}

impl Terminal {
    pub fn new(stdout: termion::raw::RawTerminal<io::Stdout>) -> Terminal {
        Self {
            row: 0,
            col: 0,
            stdout,
            last_write_col: 0,
        }
    }

    pub fn init(&mut self) {
        self.clear();
    }

    pub fn clear(&mut self) {
        self.row = 1;
        self.col = 1;
        write!(
            self.stdout,
            "{}{}",
            termion::clear::All,
            termion::cursor::Goto(self.col, self.row)
        )
        .unwrap();
        self.stdout.flush().unwrap();
    }

    pub fn next_line(&mut self) {
        self.row += 1;
        write!(self.stdout, "{}", termion::cursor::Goto(1, self.row)).unwrap();
        self.stdout.flush().unwrap();
        self.last_write_col = 1;
    }

    pub fn write(&mut self, string: &str) {
        write!(self.stdout, "{}", string).unwrap();
        self.stdout.flush().unwrap();
        let len: u16 = string.len().try_into().unwrap();
        self.last_write_col += len;
    }
}
