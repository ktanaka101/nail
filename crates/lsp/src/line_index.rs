use std::{fmt, iter};

use text_size::{TextRange, TextSize};
use tower_lsp::lsp_types;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineIndex {
    line_starts: Vec<TextSize>,
}

impl LineIndex {
    pub(crate) fn new(text: &str) -> Self {
        Self {
            line_starts: iter::once(TextSize::from(0))
                .chain(
                    text.match_indices('\n')
                        .map(|(idx, _)| TextSize::from(idx as u32 + 1)),
                )
                .collect(),
        }
    }

    pub(crate) fn line_col(&self, offset: TextSize) -> Position {
        let line = self.line_starts.partition_point(|&it| it <= offset) - 1;
        let line = LineNumber(TextSize::try_from(line).expect("line number overflow"));

        let line_start_offset = offset - self.line_starts[usize::from(line.0)];
        let col = ColNumber(line_start_offset);

        Position {
            line_number: line,
            col_number: col,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PositionRange {
    start: Position,
    end: Position,
}

impl PositionRange {
    pub fn from_text_range(text_range: TextRange, line_index: &LineIndex) -> Self {
        Self {
            start: line_index.line_col(text_range.start()),
            end: line_index.line_col(text_range.end()),
        }
    }

    pub fn start(&self) -> Position {
        self.start
    }

    pub fn end(&self) -> Position {
        self.end
    }
}

impl From<PositionRange> for lsp_types::Range {
    fn from(range: PositionRange) -> lsp_types::Range {
        lsp_types::Range {
            start: lsp_types::Position {
                line: range.start().line_number().0.into(),
                character: range.start().col_number().0.into(),
            },
            end: lsp_types::Position {
                line: range.end().line_number().0.into(),
                character: range.end().col_number().0.into(),
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    line_number: LineNumber,
    col_number: ColNumber,
}

impl Position {
    pub fn line_number(&self) -> LineNumber {
        self.line_number
    }

    pub fn col_number(&self) -> ColNumber {
        self.col_number
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineNumber(pub TextSize);

impl fmt::Display for LineNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", u32::from(self.0) + 1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ColNumber(pub TextSize);

impl fmt::Display for ColNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", u32::from(self.0) + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check<const LEN: usize>(text: &str, line_starts: [u32; LEN]) {
        assert_eq!(
            LineIndex::new(text),
            LineIndex {
                line_starts: line_starts.into_iter().map(TextSize::from).collect()
            }
        );
    }

    #[test]
    fn empty() {
        check("", [0]);
    }

    #[test]
    fn one() {
        check("\n", [0, 1]);
    }

    #[test]
    fn trailing() {
        check("foo\n", [0, 4]);
    }

    #[test]
    fn two() {
        check("foo\nbar", [0, 4]);
    }
}

#[cfg(test)]
mod tests_line_col {
    use super::*;

    fn check_line_col(text: &str, actual_offset: u32, expected_line: u32, expected_col: u32) {
        let line_index = LineIndex::new(text);
        let offset = TextSize::from(actual_offset);
        let line = LineNumber(TextSize::from(expected_line));
        let col = ColNumber(TextSize::from(expected_col));
        assert_eq!(
            line_index.line_col(offset),
            Position {
                line_number: line,
                col_number: col
            }
        );
    }

    #[test]
    fn index0() {
        check_line_col("foo\nbar\n", 0, 0, 0);
    }

    #[test]
    fn index1() {
        check_line_col("foo\nbar\n", 1, 0, 1);
    }

    #[test]
    fn index_prev_new_line() {
        check_line_col("foo\nbar\n", 2, 0, 2);
    }

    #[test]
    fn index_new_line() {
        check_line_col("foo\nbar\n", 3, 0, 3);
    }

    #[test]
    fn index_next_new_line() {
        check_line_col("foo\nbar\n", 4, 1, 0);
    }

    #[test]
    fn index_prev_two_new_line() {
        check_line_col("foo\nbar\n", 6, 1, 2);
    }

    #[test]
    fn last_index() {
        check_line_col("foo\nbar\n", 7, 1, 3);
    }
}
