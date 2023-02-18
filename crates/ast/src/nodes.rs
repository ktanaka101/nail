use syntax::{SyntaxKind, SyntaxNode, SyntaxToken};

use crate::{
    ast_node::{self, Ast, AstNode, AstToken},
    tokens,
};

macro_rules! def_ast_node {
    ($kind:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $kind {
            syntax: SyntaxNode,
        }

        impl Ast for $kind {}
        impl AstNode for $kind {
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
        ast_node::child_token(self)
    }

    pub fn value(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }
}

def_ast_node!(ExprStmt);
impl ExprStmt {
    pub fn expr(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }

    pub fn semicolon(&self) -> Option<tokens::Semicolon> {
        ast_node::child_token(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    BinaryExpr(BinaryExpr),
    Literal(Literal),
    ParenExpr(ParenExpr),
    UnaryExpr(UnaryExpr),
    PathExpr(PathExpr),
    CallExpr(CallExpr),
    BlockExpr(BlockExpr),
    IfExpr(IfExpr),
    ReturnExpr(ReturnExpr),
}
impl Ast for Expr {}
impl AstNode for Expr {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::BinaryExpr
                | SyntaxKind::Literal
                | SyntaxKind::ParenExpr
                | SyntaxKind::UnaryExpr
                | SyntaxKind::PathExpr
                | SyntaxKind::CallExpr
                | SyntaxKind::BlockExpr
                | SyntaxKind::IfExpr
                | SyntaxKind::ReturnExpr
        )
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let result = match syntax.kind() {
            SyntaxKind::BinaryExpr => Self::BinaryExpr(BinaryExpr { syntax }),
            SyntaxKind::Literal => Self::Literal(Literal { syntax }),
            SyntaxKind::ParenExpr => Self::ParenExpr(ParenExpr { syntax }),
            SyntaxKind::UnaryExpr => Self::UnaryExpr(UnaryExpr { syntax }),
            SyntaxKind::PathExpr => Self::PathExpr(PathExpr { syntax }),
            SyntaxKind::CallExpr => Self::CallExpr(CallExpr { syntax }),
            SyntaxKind::BlockExpr => Self::BlockExpr(BlockExpr { syntax }),
            SyntaxKind::IfExpr => Self::IfExpr(IfExpr { syntax }),
            SyntaxKind::ReturnExpr => Self::ReturnExpr(ReturnExpr { syntax }),
            _ => return None,
        };

        Some(result)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Expr::BinaryExpr(it) => it.syntax(),
            Expr::Literal(it) => it.syntax(),
            Expr::ParenExpr(it) => it.syntax(),
            Expr::UnaryExpr(it) => it.syntax(),
            Expr::PathExpr(it) => it.syntax(),
            Expr::CallExpr(it) => it.syntax(),
            Expr::BlockExpr(it) => it.syntax(),
            Expr::IfExpr(it) => it.syntax(),
            Expr::ReturnExpr(it) => it.syntax(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    VariableDef(VariableDef),
    ExprStmt(ExprStmt),
    Item(Item),
}
impl Ast for Stmt {}
impl AstNode for Stmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, SyntaxKind::VariableDef | SyntaxKind::ExprStmt) || Item::can_cast(kind)
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let result = match syntax.kind() {
            SyntaxKind::VariableDef => Self::VariableDef(VariableDef { syntax }),
            SyntaxKind::ExprStmt => Self::ExprStmt(ExprStmt::cast(syntax)?),
            _ => {
                if let Some(item) = Item::cast(syntax) {
                    return Some(Stmt::Item(item));
                }
                return None;
            }
        };

        Some(result)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Stmt::VariableDef(it) => it.syntax(),
            Stmt::ExprStmt(it) => it.syntax(),
            Stmt::Item(it) => it.syntax(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Item {
    FunctionDef(FunctionDef),
    Module(Module),
}
impl Ast for Item {}
impl AstNode for Item {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, SyntaxKind::FunctionDef | SyntaxKind::Module)
    }

    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let result = match syntax.kind() {
            SyntaxKind::FunctionDef => Self::FunctionDef(FunctionDef { syntax }),
            SyntaxKind::Module => Self::Module(Module { syntax }),
            _ => return None,
        };

        Some(result)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Item::FunctionDef(it) => it.syntax(),
            Item::Module(it) => it.syntax(),
        }
    }
}

def_ast_node!(BinaryExpr);
impl BinaryExpr {
    pub fn lhs(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }

    pub fn rhs(&self) -> Option<Expr> {
        ast_node::children_nodes(self).nth(1)
    }

    pub fn op(&self) -> Option<tokens::BinaryOp> {
        ast_node::child_token(self)
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
        ast_node::child_node(self)
    }
}

def_ast_node!(UnaryExpr);
impl UnaryExpr {
    pub fn expr(&self) -> Option<Expr> {
        self.syntax.children().find_map(Expr::cast)
    }

    pub fn op(&self) -> Option<tokens::UnaryOp> {
        ast_node::child_token(self)
    }
}

def_ast_node!(PathExpr);
impl PathExpr {
    pub fn path(&self) -> Option<Path> {
        ast_node::child_node(self)
    }
}

def_ast_node!(Path);
impl Path {
    pub fn segments(&self) -> impl Iterator<Item = PathSegment> {
        ast_node::children_nodes(self)
    }
}

def_ast_node!(PathSegment);
impl PathSegment {
    pub fn name(&self) -> Option<tokens::Ident> {
        ast_node::child_token(self)
    }
}

def_ast_node!(BlockExpr);
impl BlockExpr {
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> {
        ast_node::children_nodes(self)
    }
}

def_ast_node!(IfExpr);
impl IfExpr {
    pub fn condition(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }

    pub fn then_branch(&self) -> Option<BlockExpr> {
        self.children_after_condition().next()
    }

    pub fn else_branch(&self) -> Option<BlockExpr> {
        self.children_after_condition().nth(1)
    }

    fn children_after_condition<N: AstNode>(&self) -> impl Iterator<Item = N> {
        self.syntax().children().skip(1).filter_map(N::cast)
    }
}

def_ast_node!(ReturnExpr);
impl ReturnExpr {
    pub fn value(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }
}

def_ast_node!(FunctionDef);
impl FunctionDef {
    pub fn params(&self) -> Option<ParamList> {
        ast_node::child_node(self)
    }

    pub fn name(&self) -> Option<tokens::Ident> {
        ast_node::child_token(self)
    }

    pub fn body(&self) -> Option<BlockExpr> {
        ast_node::child_node(self)
    }

    pub fn return_type(&self) -> Option<ReturnType> {
        ast_node::child_node(self)
    }
}

def_ast_node!(ParamList);
impl ParamList {
    pub fn params(&self) -> impl Iterator<Item = Param> {
        ast_node::children_nodes(self)
    }
}

def_ast_node!(Param);
impl Param {
    pub fn name(&self) -> Option<tokens::Ident> {
        ast_node::child_token(self)
    }

    pub fn ty(&self) -> Option<Type> {
        ast_node::child_node(self)
    }
}

def_ast_node!(ReturnType);
impl ReturnType {
    pub fn ty(&self) -> Option<Type> {
        ast_node::child_node(self)
    }
}

def_ast_node!(Type);
impl Type {
    pub fn ty(&self) -> Option<tokens::Ident> {
        ast_node::child_token(self)
    }
}

def_ast_node!(CallExpr);
impl CallExpr {
    pub fn callee(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }

    pub fn args(&self) -> Option<ArgList> {
        ast_node::child_node(self)
    }
}

def_ast_node!(ArgList);
impl ArgList {
    pub fn args(&self) -> impl Iterator<Item = Arg> {
        ast_node::children_nodes(self)
    }
}

def_ast_node!(Arg);
impl Arg {
    pub fn expr(&self) -> Option<Expr> {
        ast_node::child_node(self)
    }
}

def_ast_node!(Module);
impl Module {
    pub fn name(&self) -> Option<tokens::Ident> {
        ast_node::child_token(self)
    }

    pub fn items(&self) -> Option<ItemList> {
        ast_node::child_node(self)
    }
}

def_ast_node!(ItemList);
impl ItemList {
    pub fn items(&self) -> impl Iterator<Item = Item> {
        ast_node::children_nodes(self)
    }
}
