use std::convert::TryInto;
use std::io;
use std::io::Write;

use termion;

pub struct Terminal {
    pub cursor_pos: Pos,
    stdout: termion::raw::RawTerminal<io::Stdout>,
}

pub struct Pos {
    row: u16,
    col: u16,
}

impl Pos {
    fn right(&mut self, num: u16) {
        self.col += num;
    }

    fn left(&mut self, num: u16) {
        self.col -= num;
    }

    fn up(&mut self, num: u16) {
        self.row -= num;
    }

    fn down(&mut self, num: u16) {
        self.row += num;
    }
}

pub enum MoveCursorAction {
    Left(u16),
}

impl Terminal {
    pub fn new(stdout: termion::raw::RawTerminal<io::Stdout>) -> Terminal {
        Self {
            cursor_pos: Pos { row: 0, col: 0 },
            stdout,
        }
    }

    pub fn init(&mut self) {
        self.clear();
    }

    pub fn clear(&mut self) {
        self.cursor_pos.col = 1;
        self.cursor_pos.row = 1;

        write!(
            self.stdout,
            "{}{}",
            termion::clear::All,
            self.get_goto_current_pos()
        )
        .unwrap();
        self.stdout.flush().unwrap();
    }

    pub fn next_line(&mut self) {
        self.cursor_pos.col = 1;
        self.cursor_pos.down(1);

        write!(self.stdout, "{}", self.get_goto_current_pos()).unwrap();
        self.stdout.flush().unwrap();
    }

    pub fn write(&mut self, string: &str) {
        write!(self.stdout, "{}", string).unwrap();
        self.stdout.flush().unwrap();
        let len: u16 = string.len().try_into().unwrap();
        self.cursor_pos.right(len);
    }

    fn get_goto_current_pos(&self) -> termion::cursor::Goto {
        termion::cursor::Goto(self.cursor_pos.col, self.cursor_pos.row)
    }
}
