use super::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub params: Vec<Identifier>,
    pub body: Block,
}

impl Display for Closure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let out = format!(
            "|{}| {}",
            self.params
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.body
        );

        write!(f, "{}", out)
    }
}

impl TryFrom<Expr> for Closure {
    type Error = Error;

    fn try_from(value: Expr) -> Result<Self> {
        match value {
            Expr::Closure(func) => Ok(func),
            expr => Err(ParserError::Convert(format!("{:?}", expr), "Closure".into()).into()),
        }
    }
}
