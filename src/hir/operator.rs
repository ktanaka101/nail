#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Equal,
    NotEqual,
    Lt,
    Gt,
}
