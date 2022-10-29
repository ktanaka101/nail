use super::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub value: String,
    pub mtype: Option<Type>,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(mtype) = &self.mtype {
            write!(f, "{} {}", self.value, mtype)
        } else {
            write!(f, "{}", self.value)
        }
    }
}

impl TryFrom<Expr> for Identifier {
    type Error = Error;

    fn try_from(value: Expr) -> Result<Self> {
        match value {
            Expr::Identifier(ident) => Ok(ident),
            expr => Err(ParserError::Convert(format!("{:?}", expr), "Identifier".into()).into()),
        }
    }
}
