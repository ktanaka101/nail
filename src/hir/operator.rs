#[derive(Debug, Clone, PartialEq)]
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
