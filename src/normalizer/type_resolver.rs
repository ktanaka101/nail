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
            ast::Expr::Identifier(id) => id.resolve_type(),
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

impl TypeResolver for ast::Identifier {
    fn resolve_type(&self) -> Option<ast::Type> {
        self.mtype.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_type_by_array() {
        let arr = ast::Array { elements: vec![] };
        assert_eq!(arr.resolve_type(), Some(ast::Type::Array));
    }

    #[test]
    fn resolve_type_by_boolean() {
        let boolean = ast::Boolean { value: false };
        assert_eq!(boolean.resolve_type(), Some(ast::Type::Boolean));
    }

    #[test]
    fn resolve_type_by_function() {
        let f = ast::Function {
            fn_type: ast::FunctionType::Function,
            params: vec![],
            name: "".into(),
            body: ast::Block { statements: vec![] },
        };
        assert_eq!(f.resolve_type(), Some(ast::Type::Function));
    }

    #[test]
    fn resolve_type_by_hash() {
        let hs = ast::Hash { pairs: vec![] };
        assert_eq!(hs.resolve_type(), Some(ast::Type::Hash));
    }

    #[test]
    fn resolve_type_by_integer() {
        let int = ast::Integer { value: 0 };
        assert_eq!(int.resolve_type(), Some(ast::Type::Integer));
    }

    #[test]
    fn resolve_type_by_char() {
        let c = ast::Char {
            value: 'a'.to_string(),
        };
        assert_eq!(c.resolve_type(), Some(ast::Type::Char));
    }

    #[test]
    fn resolve_type_by_string() {
        let string = ast::StringLit { value: "".into() };
        assert_eq!(string.resolve_type(), Some(ast::Type::String));
    }

    #[test]
    fn resolve_type_by_identifier() {
        let id = ast::Identifier {
            value: "".into(),
            mtype: Some(ast::Type::Array),
        };
        assert_eq!(id.resolve_type(), Some(ast::Type::Array));

        let id = ast::Identifier {
            value: "".into(),
            mtype: None,
        };
        assert_eq!(id.resolve_type(), None);
    }
}
