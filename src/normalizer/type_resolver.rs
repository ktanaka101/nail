use crate::parser::ast;

pub trait TypeResolver {
    fn resolve_type(&self) -> Option<ast::Type>;
}

impl TypeResolver for ast::Expr {
    fn resolve_type(&self) -> Option<ast::Type> {
        match self {
            ast::Expr::Array(arr) => arr.resolve_type(),
            ast::Expr::Boolean(b) => b.resolve_type(),
            ast::Expr::Function(f) => f.resolve_type(),
            ast::Expr::Hash(hs) => hs.resolve_type(),
            ast::Expr::Integer(int) => int.resolve_type(),
            ast::Expr::Char(c) => c.resolve_type(),
            ast::Expr::StringLit(s) => s.resolve_type(),
            _need_resolve_expr => unimplemented!(),
        }
    }
}

impl TypeResolver for ast::Array {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::Array)
    }
}

impl TypeResolver for ast::Boolean {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::Boolean)
    }
}

impl TypeResolver for ast::Function {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::Function)
    }
}

impl TypeResolver for ast::Char {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::Char)
    }
}

impl TypeResolver for ast::Hash {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::Hash)
    }
}

impl TypeResolver for ast::Integer {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::Integer)
    }
}

impl TypeResolver for ast::StringLit {
    fn resolve_type(&self) -> Option<ast::Type> {
        Some(ast::Type::String)
    }
}
