use super::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClosureParam {
    pub name: Identifier,
    pub r#type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub params: Vec<ClosureParam>,
    pub body: Block,
    pub return_type: Option<Type>,
}

impl Display for ClosureParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(r#type) = &self.r#type {
            write!(f, "{}: {}", self.name, r#type)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        let out = if let Some(ty) = &self.return_type {
            format!("|{}|: {} {}", params, ty, self.body)
        } else {
            format!("|{}| {}", params, self.body)
        };

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
