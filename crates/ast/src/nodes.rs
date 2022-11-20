use rowan::ast::AstNode;
use syntax::{NailLanguage, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

use crate::{operators, tokens, AstToken};

macro_rules! def_ast_node {
    ($kind:ident) => {
        #[derive(Clone, PartialEq, Eq, Hash)]
        pub struct $kind {
            syntax: SyntaxNode,
        }

        impl AstNode for $kind {
            type Language = NailLanguage;

            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::$kind
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self { syntax })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.syntax
            }
        }
    };
}

def_ast_node!(VariableDef);
impl VariableDef {
    pub fn name(&self) -> Option<tokens::Ident> {
        self.syntax
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find_map(tokens::Ident::cast)
    }

    pub fn value(&self) -> Option<Expr> {
        self.syntax.children().find_map(Expr::cast)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    BinaryExpr(BinaryExpr),
    Literal(Literal),
    ParenExpr(ParenExpr),
    UnaryExpr(UnaryExpr),
    VariableRef(VariableRef),
    Block(Block),
}
impl Expr {
    pub fn cast(syntax: SyntaxNode) -> Option<Self> {
        let result = match syntax.kind() {
            SyntaxKind::BinaryExpr => Self::BinaryExpr(BinaryExpr { syntax }),
            SyntaxKind::Literal => Self::Literal(Literal { syntax }),
            SyntaxKind::ParenExpr => Self::ParenExpr(ParenExpr { syntax }),
            SyntaxKind::UnaryExpr => Self::UnaryExpr(UnaryExpr { syntax }),
            SyntaxKind::VariableRef => Self::VariableRef(VariableRef { syntax }),
            SyntaxKind::Block => Self::Block(Block { syntax }),
            _ => return None,
        };

        Some(result)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    VariableDef(VariableDef),
    Expr(Expr),
    FunctionDef(FunctionDef),
}
impl Stmt {
    pub fn cast(syntax: SyntaxNode) -> Option<Self> {
        let result = match syntax.kind() {
            SyntaxKind::VariableDef => Self::VariableDef(VariableDef { syntax }),
            _ => Self::Expr(Expr::cast(syntax)?),
        };

        Some(result)
    }
}

def_ast_node!(BinaryExpr);
impl BinaryExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.syntax.children().find_map(Expr::cast)
    }

    pub fn rhs(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(1)
    }

    pub fn op(&self) -> Option<operators::BinaryOp> {
        self.syntax
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find_map(operators::BinaryOp::cast)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    Integer(tokens::Integer),
    String(tokens::String),
    Char(tokens::Char),
    Bool(tokens::Bool),
}

def_ast_node!(Literal);
impl Literal {
    fn token(&self) -> SyntaxToken {
        self.syntax.first_token().unwrap()
    }

    pub fn kind(&self) -> LiteralKind {
        let token = self.token();
        if let Some(t) = tokens::Integer::cast(token.clone()) {
            LiteralKind::Integer(t)
        } else if let Some(t) = tokens::String::cast(token.clone()) {
            LiteralKind::String(t)
        } else if let Some(t) = tokens::Char::cast(token.clone()) {
            LiteralKind::Char(t)
        } else if let Some(t) = tokens::Bool::cast(token) {
            LiteralKind::Bool(t)
        } else {
            panic!("unknown literal kind");
        }
    }
}

def_ast_node!(ParenExpr);
impl ParenExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax.children().find_map(Expr::cast)
    }
}

def_ast_node!(UnaryExpr);
impl UnaryExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax.children().find_map(Expr::cast)
    }

    pub fn op(&self) -> Option<operators::UnaryOp> {
        self.syntax
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find_map(operators::UnaryOp::cast)
    }
}

def_ast_node!(VariableRef);
impl VariableRef {
    pub fn name(&self) -> Option<tokens::Ident> {
        self.syntax
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find_map(tokens::Ident::cast)
    }
}

def_ast_node!(Block);
impl Block {
    pub fn stmts(&self) -> Vec<Stmt> {
        self.syntax.children().filter_map(Stmt::cast).collect()
    }
}

def_ast_node!(FunctionDef);
impl FunctionDef {
    pub fn name(&self) -> Option<tokens::Ident> {
        self.syntax
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find_map(tokens::Ident::cast)
    }

    pub fn body(&self) -> Option<Block> {
        self.syntax.children().find_map(Block::cast)
    }
}

def_ast_node!(ParamList);
impl ParamList {
    pub fn params(&self) -> impl Iterator<Item = Param> {
        self.syntax.children().filter_map(Param::cast)
    }
}

def_ast_node!(Param);
impl Param {
    pub fn name(&self) -> Option<tokens::Ident> {
        self.syntax
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find_map(tokens::Ident::cast)
    }
}
