use std::{convert::TryInto, io, io::Write};

use anyhow::Result;
use termion::screen::IntoAlternateScreen;

pub struct Terminal {
    cursor_pos: Pos,
    stdout: termion::screen::AlternateScreen<termion::raw::RawTerminal<io::Stdout>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Pos {
    pub row: u16,
    pub col: u16,
}

impl Pos {
    fn right(&mut self, num: u16) {
        self.col += num;
    }

    fn left(&mut self, num: u16) {
        self.col -= num;
    }

    fn down(&mut self, num: u16) {
        self.row += num;
    }
}

pub enum MoveCursorAction {
    Pos(Pos),
    Left(u16),
    Right(u16),
}

impl Terminal {
    pub fn new(stdout: termion::raw::RawTerminal<io::Stdout>) -> Result<Terminal> {
        let stdout = stdout.into_alternate_screen()?;

        Ok(Self {
            cursor_pos: Pos { row: 0, col: 0 },
            stdout,
        })
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

    pub fn clear_current_line(&mut self) {
        self.cursor_pos.col = 1;
        write!(
            self.stdout,
            "{}{}",
            termion::clear::CurrentLine,
            self.get_goto_current_pos()
        )
        .unwrap();
    }

    pub fn move_cursor(&mut self, pos: MoveCursorAction) {
        match pos {
            MoveCursorAction::Left(cnt) => {
                self.cursor_pos.left(cnt);
                write!(self.stdout, "{}", self.get_goto_current_pos()).unwrap();
                self.stdout.flush().unwrap();
            }
            MoveCursorAction::Right(cnt) => {
                self.cursor_pos.right(cnt);
                write!(self.stdout, "{}", self.get_goto_current_pos()).unwrap();
                self.stdout.flush().unwrap();
            }
            MoveCursorAction::Pos(pos) => {
                self.cursor_pos = pos;
                write!(self.stdout, "{}", self.get_goto_current_pos()).unwrap();
            }
        }
    }

    pub fn cursor_pos(&self) -> Pos {
        self.cursor_pos
    }

    fn get_goto_current_pos(&self) -> termion::cursor::Goto {
        termion::cursor::Goto(self.cursor_pos.col, self.cursor_pos.row)
    }
}
