use super::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionParam {
    pub name: Identifier,
    pub r#type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub params: Vec<FunctionParam>,
    pub body: Block,
    pub name: String,
    pub return_type: Option<Type>,
}

impl Function {
    const fn literal() -> &'static str {
        "fn"
    }
}

impl Display for FunctionParam {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let out = if let Some(ty) = &self.r#type {
            format!("{}: {}", self.name, ty)
        } else {
            format!("{}", self.name)
        };

        write!(f, "{}", out)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        let out = if let Some(ty) = &self.return_type {
            format!(
                "{} {}({}): {} {}",
                Self::literal(),
                self.name,
                params,
                ty,
                self.body
            )
        } else {
            format!(
                "{} {}({}) {}",
                Self::literal(),
                self.name,
                params,
                self.body
            )
        };

        write!(f, "{}", out)
    }
}

impl TryFrom<Expr> for Function {
    type Error = Error;

    fn try_from(value: Expr) -> Result<Self> {
        match value {
            Expr::Function(func) => Ok(func),
            expr => Err(ParserError::Convert(format!("{:?}", expr), "Function".into()).into()),
        }
    }
}
