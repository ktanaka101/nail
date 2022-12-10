use super::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub name: Identifier,
    pub value: Expr,
    pub r#type: Option<Type>,
}

impl Display for Let {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(r#type) = &self.r#type {
            write!(f, "{}: {} = {}", self.name, r#type, self.value)
        } else {
            write!(f, "{} = {}", self.name, self.value)
        }
    }
}

impl TryFrom<Stmt> for Let {
    type Error = Error;

    fn try_from(value: Stmt) -> Result<Self> {
        match value {
            Stmt::Let(mlet) => Ok(mlet),
            stmt => Err(ParserError::Convert(format!("{stmt:?}"), "Let".into()).into()),
        }
    }
}
