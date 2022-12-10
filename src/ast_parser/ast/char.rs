use super::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Char {
    pub value: String,
}

impl Display for Char {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl TryFrom<Expr> for Char {
    type Error = Error;

    fn try_from(value: Expr) -> Result<Self> {
        match value {
            Expr::Char(c) => Ok(c),
            expr => Err(ParserError::Convert(format!("{expr:?}"), "Char".into()).into()),
        }
    }
}
