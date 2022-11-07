use std::iter;

use text_size::TextSize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LineIndex {
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
        let col = ColNumber(offset - line_start_offset);

        Position {
            line_number: line,
            col_number: col,
        }
    }
}

pub(crate) struct Position {
    line_number: LineNumber,
    col_number: ColNumber,
}

impl Position {
    pub(crate) fn line_number(&self) -> LineNumber {
        self.line_number
    }

    pub(crate) fn col_number(&self) -> ColNumber {
        self.col_number
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct LineNumber(TextSize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ColNumber(TextSize);

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
