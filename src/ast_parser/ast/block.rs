use super::{prelude::*, Tokens};

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub tokens: Tokens,
}

impl Block {
    pub fn last_stmt(&self) -> Option<Stmt> {
        Some(self.statements.last()?.to_owned())
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{ {} }}",
            self.statements
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join("")
        )
    }
}

impl TryFrom<Stmt> for Block {
    type Error = Error;

    fn try_from(value: Stmt) -> Result<Self> {
        match value {
            Stmt::Block(block) => Ok(block),
            stmt => Err(ParserError::Convert(format!("{:?}", stmt), "Block".into()).into()),
        }
    }
}
