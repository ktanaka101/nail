pub mod ast;
pub mod error;

use std::str::FromStr;

use crate::token::Token;

use anyhow::Result;

use ast::{Expr, Stmt};
use error::ParserError;

#[derive(PartialOrd, PartialEq, Debug)]
enum Priority {
    Lowest,
    Equals,
    Lessgreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl From<&Token> for Priority {
    fn from(value: &Token) -> Priority {
        match value {
            Token::Equal(_) => Priority::Equals,
            Token::NotEqual(_) => Priority::Equals,
            Token::Lt(_) => Priority::Lessgreater,
            Token::Gt(_) => Priority::Lessgreater,
            Token::Plus(_) => Priority::Sum,
            Token::Minus(_) => Priority::Sum,
            Token::Slash(_) => Priority::Product,
            Token::Asterisk(_) => Priority::Product,
            Token::Lparen(_) => Priority::Call,
            Token::Lbracket(_) => Priority::Index,
            _ => Priority::Lowest,
        }
    }
}

#[derive(Debug)]
pub struct Parser<T: Lexer> {
    lexer: T,
    cur_token: Token,
    peek_token: Token,
    cur_tokens: Vec<Token>,
    pub errors: Vec<String>,
}

#[derive(Debug)]
enum InfixFn {
    Infix,
    Call,
    Index,
}

fn token_to_operator(t: &Token) -> Result<ast::Operator> {
    Ok(match t {
        Token::Assign(_) => ast::Operator::Assign,
        Token::Plus(_) => ast::Operator::Plus,
        Token::Minus(_) => ast::Operator::Minus,
        Token::Bang(_) => ast::Operator::Bang,
        Token::Asterisk(_) => ast::Operator::Asterisk,
        Token::Slash(_) => ast::Operator::Slash,
        Token::Equal(_) => ast::Operator::Equal,
        Token::NotEqual(_) => ast::Operator::NotEqual,
        Token::Lt(_) => ast::Operator::Lt,
        Token::Gt(_) => ast::Operator::Gt,
        t => return Err(ParserError::ExpectOperator(format!("{:?}", t)).into()),
    })
}

pub trait Lexer {
    fn next_token(&mut self) -> Token;
}

impl<T: Lexer> Parser<T> {
    pub fn new(mut lexer: T) -> Parser<T> {
        Parser {
            cur_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            cur_tokens: Vec::<Token>::new(),
            lexer,
            errors: Vec::new(),
        }
    }

    fn next_token(&mut self) {
        self.cur_tokens.push(self.cur_token.clone());
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Result<ast::Program> {
        let mut program = ast::Program::default();

        while self.cur_token != Token::eof() {
            let stmt = self.parse_statement()?;
            program.statements.push(stmt);
            self.next_token();
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Stmt> {
        Ok(match self.cur_token {
            Token::Let(_) => Stmt::Let(self.parse_let_statement()?),
            Token::Return(_) => Stmt::Return(self.parse_return_statement()?),
            _ => Stmt::ExprStmt(self.parse_expr_statement()?),
        })
    }

    fn parse_let_statement(&mut self) -> Result<ast::Let> {
        self.expect_peek(Token::ident())?;
        let ident = match &self.cur_token {
            Token::Ident(id) => id.clone(),
            _ => unreachable!(),
        };

        let r#type = if self.peek_token_is(Token::colon()) {
            self.next_token();
            self.next_token();
            Some(self.parse_type()?)
        } else {
            None
        };

        let name = ast::Identifier { value: ident.input };

        self.expect_peek(Token::assign())?;

        self.next_token();
        let value = self.parse_expr(Priority::Lowest)?;
        self.optional_peek(Token::semicolon());

        let value = if let Expr::Function(mut func) = value {
            func.name = name.value.clone();
            func.into()
        } else {
            value
        };

        Ok(ast::Let {
            name,
            value,
            r#type,
        })
    }

    fn parse_return_statement(&mut self) -> Result<ast::Return> {
        self.next_token();

        let return_value = self.parse_expr(Priority::Lowest)?;

        self.optional_peek(Token::semicolon());

        Ok(ast::Return { return_value })
    }

    fn parse_expr_statement(&mut self) -> Result<ast::ExprStmt> {
        let expr = self.parse_expr(Priority::Lowest)?;

        self.optional_peek(Token::semicolon());

        Ok(ast::ExprStmt { expr })
    }

    fn parse_expr(&mut self, precedende: Priority) -> Result<Expr> {
        match &self.cur_token {
            t @ Token::Illegal(_)
            | t @ Token::Eof(_)
            | t @ Token::Assign(_)
            | t @ Token::Plus(_)
            | t @ Token::Asterisk(_)
            | t @ Token::Slash(_)
            | t @ Token::Equal(_)
            | t @ Token::NotEqual(_)
            | t @ Token::Lt(_)
            | t @ Token::Gt(_)
            | t @ Token::Comma(_)
            | t @ Token::Semicolon(_)
            | t @ Token::Colon(_)
            | t @ Token::Rparen(_)
            | t @ Token::Rbrace(_)
            | t @ Token::Rbracket(_)
            | t @ Token::Let(_)
            | t @ Token::Else(_)
            | t @ Token::Return(_) => {
                return Err(ParserError::ExpectExpression(format!("{:?}", t)).into())
            }
            Token::Ident(_)
            | Token::Int(_)
            | Token::Bang(_)
            | Token::Minus(_)
            | Token::True(_)
            | Token::False(_)
            | Token::Char(_)
            | Token::Lparen(_)
            | Token::If(_)
            | Token::StringLiteral(_)
            | Token::Lbracket(_)
            | Token::Lbrace(_)
            | Token::Function(_)
            | Token::VerticalBar(_) => (),
        }

        let mut left_expr = self.prefix_parse_fns(self.cur_token.clone())?;

        while !self.peek_token_is(Token::semicolon())
            && precedende < Priority::from(&self.peek_token)
        {
            let infix_fn = self.infix_parse_fns(self.peek_token.clone())?;

            self.next_token();

            left_expr = match infix_fn {
                InfixFn::Infix => Expr::InfixExpr(self.parse_infix_expr(left_expr)?),
                InfixFn::Call => Expr::Call(self.parse_call_expr(left_expr)?),
                InfixFn::Index => Expr::Index(self.parse_index_expr(left_expr)?),
            };
        }

        Ok(left_expr)
    }

    fn parse_identifier(&self) -> Result<ast::Identifier> {
        Ok(ast::Identifier {
            value: match &self.cur_token {
                Token::Ident(val) => val.input.clone(),
                t => return Err(ParserError::ExpectIdentifier(format!("{:?}", t)).into()),
            },
        })
    }

    fn parse_integer_literal(&self) -> Result<ast::Integer> {
        let value = match &self.cur_token {
            Token::Int(int) => int
                .input
                .parse::<i64>()
                .map_err(|e| ParserError::InvalidInteger(format!("{:?}", e)))?,
            t => return Err(ParserError::InvalidIntegerLiteral(format!("{:?}", t)).into()),
        };

        Ok(ast::Integer { value })
    }

    fn parse_bool_literal(&self) -> Result<ast::Boolean> {
        match &self.cur_token {
            Token::True(_) => Ok(ast::Boolean { value: true }),
            Token::False(_) => Ok(ast::Boolean { value: false }),
            t => Err(ParserError::InvalidBooleanLiteral(format!("{:?}", t)).into()),
        }
    }

    fn parse_grouped_expr(&mut self) -> Result<Expr> {
        self.next_token();
        let expr = self.parse_expr(Priority::Lowest)?;
        self.expect_peek(Token::rparen())?;
        Ok(expr)
    }

    fn parse_prefix_expr(&mut self) -> Result<ast::PrefixExpr> {
        let ope = token_to_operator(&self.cur_token)?;
        self.next_token();
        let right = Box::new(self.parse_expr(Priority::Prefix)?);

        Ok(ast::PrefixExpr { ope, right })
    }

    fn parse_infix_expr(&mut self, left: Expr) -> Result<ast::InfixExpr> {
        let ope = token_to_operator(&self.cur_token)?;
        let pre = Priority::from(&self.cur_token);

        self.next_token();

        let right = Box::new(self.parse_expr(pre)?);

        Ok(ast::InfixExpr {
            left: Box::new(left),
            ope,
            right,
        })
    }

    fn parse_if_expr(&mut self) -> Result<ast::If> {
        self.optional_peek(Token::lparen());

        self.next_token();

        let cond = Box::new(self.parse_expr(Priority::Lowest)?);

        self.optional_peek(Token::rparen());

        self.expect_peek(Token::lbrace())?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(Token::r#else()) {
            self.next_token();
            self.expect_peek(Token::lbrace())?;

            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(ast::If {
            cond,
            consequence,
            alternative,
            r#type: None,
        })
    }

    fn parse_block_statement(&mut self) -> Result<ast::Block> {
        let mut statements = Vec::<Stmt>::new();

        self.next_token();

        while !(self.cur_token_is(Token::rbrace()) || self.cur_token_is(Token::eof())) {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(ast::Block {
            statements,
            tokens: self.cur_tokens.clone().into(),
        })
    }

    fn parse_function_literal(&mut self) -> Result<ast::Function> {
        self.expect_peek(Token::ident())?;
        let name = self.parse_identifier()?;

        self.expect_peek(Token::lparen())?;

        let params = self.parse_function_params()?;
        let return_type = if self.peek_token_is(Token::colon()) {
            self.next_token();
            self.next_token();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect_peek(Token::lbrace())?;

        let body = self.parse_block_statement()?;

        Ok(ast::Function {
            params,
            body,
            name: name.value,
            return_type,
        })
    }

    fn parse_function_params(&mut self) -> Result<Vec<ast::FunctionParam>> {
        let mut params = Vec::<ast::FunctionParam>::new();

        if self.peek_token_is(Token::rparen()) {
            self.next_token();
            return Ok(params);
        }

        self.next_token();

        params.push(self.parse_param()?);

        while self.peek_token_is(Token::comma()) {
            self.next_token();
            self.next_token();

            params.push(self.parse_param()?);
        }
        self.expect_peek(Token::rparen())?;

        Ok(params)
    }

    fn parse_param(&mut self) -> Result<ast::FunctionParam> {
        let identifier = ast::Identifier {
            value: match &self.cur_token {
                Token::Ident(t) => t.input.clone(),
                t => return Err(ParserError::InvalidFunctionParam(format!("{:?}", t)).into()),
            },
        };

        let r#type = if self.peek_token_is(Token::colon()) {
            self.next_token();
            self.next_token();
            Some(self.parse_type()?)
        } else {
            None
        };

        Ok(ast::FunctionParam {
            name: identifier,
            r#type,
        })
    }

    fn parse_closure_literal(&mut self) -> Result<ast::Closure> {
        let params = self.parse_closure_params()?;
        let return_type = if self.peek_token_is(Token::colon()) {
            self.next_token();
            self.next_token();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect_peek(Token::lbrace())?;

        let body = self.parse_block_statement()?;

        Ok(ast::Closure {
            params,
            body,
            return_type,
        })
    }

    fn parse_closure_params(&mut self) -> Result<Vec<ast::ClosureParam>> {
        let mut params = Vec::<ast::ClosureParam>::new();

        if self.peek_token_is(Token::vertical_bar()) {
            self.next_token();
            return Ok(params);
        }

        self.next_token();

        params.push(self.parse_closure_param()?);

        while self.peek_token_is(Token::comma()) {
            self.next_token();
            self.next_token();
            params.push(self.parse_closure_param()?);
        }
        self.expect_peek(Token::vertical_bar())?;

        Ok(params)
    }

    fn parse_closure_param(&mut self) -> Result<ast::ClosureParam> {
        let identifier = ast::Identifier {
            value: match &self.cur_token {
                Token::Ident(t) => t.input.clone(),
                t => return Err(ParserError::InvalidFunctionParam(format!("{:?}", t)).into()),
            },
        };

        let r#type = if self.peek_token_is(Token::colon()) {
            self.next_token();
            self.next_token();
            Some(self.parse_type()?)
        } else {
            None
        };

        Ok(ast::ClosureParam {
            name: identifier,
            r#type,
        })
    }

    fn parse_call_expr(&mut self, func: Expr) -> Result<ast::Call> {
        Ok(ast::Call {
            func: Box::new(func),
            args: self.parse_expr_list(Token::rparen())?,
        })
    }

    fn parse_expr_list(&mut self, end_token_t: Token) -> Result<Vec<Expr>> {
        let mut expr_list = Vec::<Expr>::new();

        if self.peek_token_is(end_token_t.clone()) {
            self.next_token();
            return Ok(expr_list);
        }

        self.next_token();
        expr_list.push(self.parse_expr(Priority::Lowest)?);

        while self.peek_token_is(Token::comma()) {
            self.next_token();
            self.next_token();
            expr_list.push(self.parse_expr(Priority::Lowest)?);
        }

        self.expect_peek(end_token_t)?;

        Ok(expr_list)
    }

    fn parse_string_literal(&self) -> Result<ast::StringLit> {
        Ok(ast::StringLit {
            value: match &self.cur_token {
                Token::StringLiteral(s) => s.input.clone(),
                t => return Err(ParserError::InvalidStringLiteral(format!("{:?}", t)).into()),
            },
        })
    }

    fn parse_char(&self) -> Result<ast::Char> {
        let s = match &self.cur_token {
            Token::Char(c) => c.input.clone(),
            t => return Err(ParserError::ExpectChar(format!("{:?}", t)).into()),
        };

        let c = char::from_str(s.as_str()).map_err(|_| ParserError::InvalidChar(s))?;

        Ok(ast::Char {
            value: c.to_string(),
        })
    }

    fn parse_array_literal(&mut self) -> Result<ast::Array> {
        self.cur_tokens.clear();

        let elements = self.parse_expr_list(Token::rbracket())?;
        self.cur_tokens.push(self.cur_token.clone());
        Ok(ast::Array {
            elements,
            tokens: self.cur_tokens.clone().into(),
        })
    }

    fn parse_index_expr(&mut self, left: Expr) -> Result<ast::Index> {
        self.next_token();
        let index = self.parse_expr(Priority::Lowest)?;
        self.expect_peek(Token::rbracket())?;

        Ok(ast::Index {
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    fn parse_hash_literal(&mut self) -> Result<ast::Hash> {
        let mut pairs = Vec::<ast::Pair>::new();

        while !self.peek_token_is(Token::rbrace()) {
            self.next_token();
            let key = self.parse_expr(Priority::Lowest)?;
            self.expect_peek(Token::colon())?;
            self.next_token();
            let value = self.parse_expr(Priority::Lowest)?;
            pairs.push(ast::Pair { key, value });

            if !self.peek_token_is(Token::rbrace()) && self.expect_peek(Token::comma()).is_err() {
                return Err(
                    ParserError::InvalidHashLiteral(format!("{:?}", self.cur_token)).into(),
                );
            }
        }
        self.expect_peek(Token::rbrace())?;

        Ok(ast::Hash { pairs })
    }

    fn parse_type(&mut self) -> Result<ast::Type> {
        match &self.cur_token {
            Token::Ident(ident) => Ok(match ident.input.as_str() {
                "array" => ast::Type::Array,
                "bool" => ast::Type::Boolean,
                "int" => ast::Type::Integer,
                "char" => ast::Type::Char,
                "string" => ast::Type::String,
                "hash" => ast::Type::Hash,
                "Fn" => ast::Type::Function,
                "never" => ast::Type::Never,
                "unit" => ast::Type::Unit,
                name => ast::Type::Custom(name.to_string()),
            }),
            _ => anyhow::bail!(ParserError::InvalidType(format!("{:?}", self.cur_token))),
        }
    }

    fn cur_token_is(&self, token_t: Token) -> bool {
        match token_t {
            Token::Illegal(_) => matches!(self.cur_token, Token::Illegal(_)),
            Token::Ident(_) => matches!(self.cur_token, Token::Ident(_)),
            Token::Int(_) => matches!(self.cur_token, Token::Int(_)),
            Token::StringLiteral(_) => matches!(self.cur_token, Token::StringLiteral(_)),
            t => self.cur_token == t,
        }
    }

    fn peek_token_is(&self, token_t: Token) -> bool {
        match token_t {
            Token::Illegal(_) => matches!(self.peek_token, Token::Illegal(_)),
            Token::Ident(_) => matches!(self.peek_token, Token::Ident(_)),
            Token::Int(_) => matches!(self.peek_token, Token::Int(_)),
            Token::StringLiteral(_) => matches!(self.peek_token, Token::StringLiteral(_)),
            t => self.peek_token == t,
        }
    }

    fn expect_peek(&mut self, token_t: Token) -> Result<()> {
        if self.peek_token_is(token_t.clone()) {
            self.next_token();
            Ok(())
        } else {
            Err(ParserError::ExpectPeek(
                format!("{:?}", &token_t),
                format!("{:?}", &self.peek_token),
            )
            .into())
        }
    }

    fn prefix_parse_fns(&mut self, token_t: Token) -> Result<Expr> {
        Ok(match token_t {
            Token::Ident(_) => Expr::Identifier(self.parse_identifier()?),
            Token::Int(_) => Expr::Integer(self.parse_integer_literal()?),
            Token::Bang(_) | Token::Minus(_) => Expr::PrefixExpr(self.parse_prefix_expr()?),
            Token::True(_) | Token::False(_) => Expr::Boolean(self.parse_bool_literal()?),
            Token::Lparen(_) => self.parse_grouped_expr()?,
            Token::If(_) => Expr::If(self.parse_if_expr()?),
            Token::Function(_) => Expr::Function(self.parse_function_literal()?),
            Token::VerticalBar(_) => Expr::Closure(self.parse_closure_literal()?),
            Token::StringLiteral(_) => Expr::StringLit(self.parse_string_literal()?),
            Token::Char(_) => Expr::Char(self.parse_char()?),
            Token::Lbracket(_) => Expr::Array(self.parse_array_literal()?),
            Token::Lbrace(_) => Expr::Hash(self.parse_hash_literal()?),
            t => return Err(ParserError::InvalidPrefix(format!("{:?}", t)).into()),
        })
    }

    fn infix_parse_fns(&self, token_t: Token) -> Result<InfixFn> {
        Ok(match token_t {
            Token::Plus(_)
            | Token::Minus(_)
            | Token::Slash(_)
            | Token::Asterisk(_)
            | Token::Equal(_)
            | Token::NotEqual(_)
            | Token::Lt(_)
            | Token::Gt(_) => InfixFn::Infix,
            Token::Lparen(_) => InfixFn::Call,
            Token::Lbracket(_) => InfixFn::Index,
            t => return Err(ParserError::InvalidInfix(format!("{:?}", t)).into()),
        })
    }

    fn optional_peek(&mut self, token: Token) -> bool {
        if self.peek_token_is(token) {
            self.next_token();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, token::Position};

    struct Id<'a>(&'a str);

    enum Val<'a> {
        S(&'a str),
        B(bool),
        Id(Id<'a>),
        I(i64),
        Infix(Box<Val<'a>>, &'a str, Box<Val<'a>>),
        C(char),
    }

    #[test]
    fn test_let_stmts() {
        let inputs = vec![
            ("let x = 5", Id("x"), None, Val::I(5)),
            ("let x = 5;", Id("x"), None, Val::I(5)),
            ("let y = true;", Id("y"), None, Val::B(true)),
            ("let foobar = y;", Id("foobar"), None, Val::Id(Id("y"))),
            ("let x = 'a'", Id("x"), None, Val::C('a')),
            (
                "let x: array = 'a'",
                Id("x"),
                Some(ast::Type::Array),
                Val::C('a'),
            ),
        ];

        for (input, id, ty, v) in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);
            test_let_by_stmt(&program.statements[0], &id, &ty, &v);
        }
    }

    #[test]
    fn test_return_stmts() {
        let inputs = vec![
            ("return 5", Val::I(5)),
            ("return 5;", Val::I(5)),
            ("return true;", Val::B(true)),
            ("return y;", Val::Id(Id("y"))),
        ];

        for (input, v) in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);
            test_return_by_stmt(&program.statements[0], &v);
        }
    }

    #[test]
    fn test_identifier_exprs() {
        let input = "foobar";
        let program = test_parse(input);
        assert_eq!(program.statements.len(), 1);
        test_identifier_by_stmt(&program.statements[0], "foobar");
    }

    #[test]
    fn test_integer_exprs() {
        let inputs = vec![("5", 5), ("5;", 5)];

        for (input, v) in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);
            test_interger_by_stmt(&program.statements[0], v);
        }
    }

    #[test]
    fn test_boolean_exprs() {
        let inputs = vec![("true", true), ("true;", true), ("false;", false)];

        for (input, v) in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);
            test_boolean_by_stmt(&program.statements[0], v);
        }
    }

    #[test]
    fn test_char_exprs() {
        let inputs = vec![("'a'", 'a'), ("'1'", '1'), ("'a';", 'a')];

        for (input, v) in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);
            test_char_by_stmt(&program.statements[0], v);
        }
    }

    #[test]
    fn test_char_expr_errors() {
        let inputs = vec![("'aa'", ParserError::InvalidChar("aa".to_string()))];
        test_errors(inputs);
    }

    #[test]
    fn test_prefix_exprs() {
        let inputs = vec![
            ("!5", "!", Val::I(5)),
            ("!5;", "!", Val::I(5)),
            ("-15;", "-", Val::I(15)),
            ("!true;", "!", Val::B(true)),
            ("!false;", "!", Val::B(false)),
        ];

        for (input, ope, v) in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);
            test_prefix_by_stmt(&program.statements[0], ope, &v);
        }
    }

    #[test]
    fn test_infix_exprs() {
        let inputs = vec![
            ("5 + 5", Val::I(5), "+", Val::I(5)),
            ("5 + 5;", Val::I(5), "+", Val::I(5)),
            ("5 - 5;", Val::I(5), "-", Val::I(5)),
            ("5 * 5;", Val::I(5), "*", Val::I(5)),
            ("5 / 5;", Val::I(5), "/", Val::I(5)),
            ("5 > 5;", Val::I(5), ">", Val::I(5)),
            ("5 < 5;", Val::I(5), "<", Val::I(5)),
            ("5 == 5;", Val::I(5), "==", Val::I(5)),
            ("5 != 5;", Val::I(5), "!=", Val::I(5)),
            ("true == true;", Val::B(true), "==", Val::B(true)),
            ("true != false;", Val::B(true), "!=", Val::B(false)),
            ("false == false;", Val::B(false), "==", Val::B(false)),
        ];

        for (input, left, ope, right) in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);
            test_infix_by_stmt(&program.statements[0], &left, ope, &right);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let inputs = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f  + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        inputs.iter().for_each(|(input, expected)| {
            let program = test_parse(input);
            let actual = program.to_string();
            assert_eq!(&actual.as_str(), expected);
        });
    }

    #[test]
    fn test_if_exprs() {
        let input = "if (x < y) { x }";

        let program = test_parse(input);
        assert_eq!(program.statements.len(), 1);

        let expr_stmt = if let Stmt::ExprStmt(x) = &program.statements[0] {
            x
        } else {
            panic!("Expect type is Stmt::ExprStmt");
        };

        let if_expr = if let Expr::If(x) = &expr_stmt.expr {
            x
        } else {
            panic!("Expect type is Expr::If");
        };

        test_infix_by_expr(
            if_expr.cond.as_ref(),
            &Val::Id(Id("x")),
            "<",
            &Val::Id(Id("y")),
        );

        assert_eq!(if_expr.consequence.statements.len(), 1);
        test_identifier_by_stmt(&if_expr.consequence.statements[0], "x");
        assert_eq!(if_expr.alternative, None);
    }

    #[test]
    fn test_if_else_exprs() {
        let inputs = vec![
            "if (x < y) { x } else { y }",
            "if (x < y) { x; } else { y; }",
            "if x < y { x; } else { y; }",
        ];

        for input in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);

            let expr_stmt = if let Stmt::ExprStmt(x) = &program.statements[0] {
                x
            } else {
                panic!("Expect type is Stmt::ExprStmt");
            };

            let if_expr = if let Expr::If(x) = &expr_stmt.expr {
                x
            } else {
                panic!("Expect type is Expr::If");
            };

            test_infix_by_expr(
                if_expr.cond.as_ref(),
                &Val::Id(Id("x")),
                "<",
                &Val::Id(Id("y")),
            );

            assert_eq!(if_expr.consequence.statements.len(), 1);
            test_identifier_by_stmt(&if_expr.consequence.statements[0], "x");

            let alt = if let Some(x) = &if_expr.alternative {
                x
            } else {
                panic!("Expect some Expr::Block");
            };
            assert_eq!(alt.statements.len(), 1);
            test_identifier_by_stmt(&alt.statements[0], "y");
        }
    }

    #[test]
    fn test_function_exprs() {
        let inputs = vec![
            "fn test_func(x: int, y: char) { x + y }",
            "fn test_func(x: int, y: char) { x + y; }",
        ];

        for input in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);

            let expr_stmt = if let Stmt::ExprStmt(x) = &program.statements[0] {
                x
            } else {
                panic!("Expect type is Stmt::ExprStmt");
            };

            let fn_expr = if let Expr::Function(x) = &expr_stmt.expr {
                x
            } else {
                panic!("Expect type is Expr::Function")
            };

            assert_eq!(fn_expr.name, "test_func");

            assert_eq!(fn_expr.params.len(), 2);
            test_function_param(&fn_expr.params[0], "x", &Some(ast::Type::Integer));
            test_function_param(&fn_expr.params[1], "y", &Some(ast::Type::Char));

            assert_eq!(fn_expr.body.statements.len(), 1);
            test_infix_by_stmt(
                &fn_expr.body.statements[0],
                &Val::Id(Id("x")),
                "+",
                &Val::Id(Id("y")),
            )
        }
    }

    #[test]
    fn test_closure_exprs() {
        let inputs = vec!["|x, y| { x + y }", "|x, y| { x + y; }"];

        for input in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);

            let expr_stmt = if let Stmt::ExprStmt(x) = &program.statements[0] {
                x
            } else {
                panic!("Expect type is Stmt::ExprStmt");
            };

            let cl_expr = if let Expr::Closure(x) = &expr_stmt.expr {
                x
            } else {
                panic!("Expect type is Expr::Closure")
            };

            assert_eq!(cl_expr.params.len(), 2);
            test_closure_param(&cl_expr.params[0], "x", &None);
            test_closure_param(&cl_expr.params[1], "y", &None);

            assert_eq!(cl_expr.body.statements.len(), 1);
            test_infix_by_stmt(
                &cl_expr.body.statements[0],
                &Val::Id(Id("x")),
                "+",
                &Val::Id(Id("y")),
            )
        }
    }

    #[test]
    fn test_closure_params() {
        let inputs = vec![
            ("|| {};", vec![]),
            ("|x| {};", vec![("x", None)]),
            ("|x, y, z| {};", vec![("x", None), ("y", None), ("z", None)]),
            (
                "|x: int, y: char, z| {};",
                vec![
                    ("x", Some(ast::Type::Integer)),
                    ("y", Some(ast::Type::Char)),
                    ("z", None),
                ],
            ),
        ];

        for (input, ids) in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);

            let stmt_expr = if let Stmt::ExprStmt(x) = &program.statements[0] {
                x
            } else {
                panic!("Expect type is Stmt::ExprStmt.");
            };

            let fn_expr = if let Expr::Closure(x) = &stmt_expr.expr {
                x
            } else {
                panic!("Expect type is Expr::Closure.");
            };

            assert_eq!(fn_expr.params.len(), ids.len());

            for (i, (id, ty)) in ids.into_iter().enumerate() {
                test_closure_param(&fn_expr.params[i], id, &ty);
            }
        }
    }

    #[test]
    fn test_call_exprs() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let program = test_parse(input);
        assert_eq!(program.statements.len(), 1);

        let stmt_expr = if let Stmt::ExprStmt(x) = &program.statements[0] {
            x
        } else {
            panic!("Expect type is Stmt::ExprStmt");
        };

        let call_expr = if let Expr::Call(x) = &stmt_expr.expr {
            x
        } else {
            panic!("Expect type is Expr::Call");
        };

        test_expr(call_expr.func.as_ref(), &Val::Id(Id("add")));
        assert_eq!(call_expr.args.len(), 3);
        test_expr(&call_expr.args[0], &Val::I(1));
        test_infix_by_expr(&call_expr.args[1], &Val::I(2), "*", &Val::I(3));
        test_infix_by_expr(&call_expr.args[2], &Val::I(4), "+", &Val::I(5));
    }

    #[test]
    fn test_string_exprs() {
        let input = "\"hello world\"";
        let program = test_parse(input);
        assert_eq!(program.statements.len(), 1);

        let stmt_expr = if let Stmt::ExprStmt(x) = &program.statements[0] {
            x
        } else {
            panic!("Expect type is Stmt::ExprStmt.");
        };

        test_expr(&stmt_expr.expr, &Val::S("hello world"));
    }

    #[test]
    fn test_array_expr() {
        let input = "[1, 2 * 2, 3 + 3]";
        let program = test_parse(input);
        assert_eq!(program.statements.len(), 1);

        let stmt_expr = if let Stmt::ExprStmt(x) = &program.statements[0] {
            x
        } else {
            panic!("Expect type is Stmt::ExprStmt.");
        };

        let array_expr = if let Expr::Array(x) = &stmt_expr.expr {
            x
        } else {
            panic!("Expect type is Expr::Array.");
        };
        assert_eq!(array_expr.elements.len(), 3);
        test_expr(&array_expr.elements[0], &Val::I(1));
        test_infix_by_expr(&array_expr.elements[1], &Val::I(2), "*", &Val::I(2));
        test_infix_by_expr(&array_expr.elements[2], &Val::I(3), "+", &Val::I(3));

        let expected_tokens = vec![
            Token::lbracket(),
            Token::int_with("1".to_string(), Position::default()),
            Token::comma(),
            Token::int_with("2".to_string(), Position::default()),
            Token::asterisk(),
            Token::int_with("2".to_string(), Position::default()),
            Token::comma(),
            Token::int_with("3".to_string(), Position::default()),
            Token::plus(),
            Token::int_with("3".to_string(), Position::default()),
            Token::rbracket(),
        ];
        assert_eq!(array_expr.tokens, expected_tokens.into());
    }

    #[test]
    fn test_index_expr() {
        let input = "myArray[1 + 1]";
        let program = test_parse(input);
        assert_eq!(program.statements.len(), 1);

        let stmt_expr = if let Stmt::ExprStmt(x) = &program.statements[0] {
            x
        } else {
            panic!("Expect type is Stmt::ExprStmt.");
        };

        let index_expr = if let Expr::Index(x) = &stmt_expr.expr {
            x
        } else {
            panic!("Expect type is Expr::Index.");
        };
        test_expr(index_expr.left.as_ref(), &Val::Id(Id("myArray")));
        test_infix_by_expr(index_expr.index.as_ref(), &Val::I(1), "+", &Val::I(1));
    }

    #[test]
    fn test_hash_expr() {
        let inputs = vec![
            ("{}", vec![]),
            (
                r#"{"one": 1, "two": 2, "three": 3}"#,
                vec![
                    (Val::S("one"), Val::I(1)),
                    (Val::S("two"), Val::I(2)),
                    (Val::S("three"), Val::I(3)),
                ],
            ),
            (
                r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#,
                vec![
                    (
                        Val::S("one"),
                        Val::Infix(Box::new(Val::I(0)), "+", Box::new(Val::I(1))),
                    ),
                    (
                        Val::S("two"),
                        Val::Infix(Box::new(Val::I(10)), "-", Box::new(Val::I(8))),
                    ),
                    (
                        Val::S("three"),
                        Val::Infix(Box::new(Val::I(15)), "/", Box::new(Val::I(5))),
                    ),
                ],
            ),
            (
                r#"{1: 111, 2: "b", 3: true}"#,
                vec![
                    (Val::I(1), Val::I(111)),
                    (Val::I(2), Val::S("b")),
                    (Val::I(3), Val::B(true)),
                ],
            ),
            (
                r#"{true: 1, false: "abc"}"#,
                vec![(Val::B(true), Val::I(1)), (Val::B(false), Val::S("abc"))],
            ),
        ];

        for (input, expect_list) in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);

            let expr_stmt = if let Stmt::ExprStmt(x) = &program.statements[0] {
                x
            } else {
                panic!("Expect type is Stmt::ExprStmt.");
            };

            let hash_expr = if let Expr::Hash(x) = &expr_stmt.expr {
                x
            } else {
                panic!("Expect type is Expr::Hash.");
            };
            assert_eq!(hash_expr.pairs.len(), expect_list.len());
            for (i, expect) in expect_list.into_iter().enumerate() {
                test_expr(&hash_expr.pairs[i].key, &expect.0);
                test_expr(&hash_expr.pairs[i].value, &expect.1);
            }
        }
    }

    #[test]
    fn test_type_by_let_stmts() {
        let inputs = vec![
            (
                "let x: array = 'a'",
                Id("x"),
                Some(ast::Type::Array),
                Val::C('a'),
            ),
            (
                "let x: bool = 'a'",
                Id("x"),
                Some(ast::Type::Boolean),
                Val::C('a'),
            ),
            (
                "let x: Fn = 'a'",
                Id("x"),
                Some(ast::Type::Function),
                Val::C('a'),
            ),
            (
                "let x: hash = 'a'",
                Id("x"),
                Some(ast::Type::Hash),
                Val::C('a'),
            ),
            (
                "let x: int = 'a'",
                Id("x"),
                Some(ast::Type::Integer),
                Val::C('a'),
            ),
            (
                "let x: char = 'a'",
                Id("x"),
                Some(ast::Type::Char),
                Val::C('a'),
            ),
            (
                "let x: string = 'a'",
                Id("x"),
                Some(ast::Type::String),
                Val::C('a'),
            ),
            (
                "let x: CustomeType = 'a'",
                Id("x"),
                Some(ast::Type::Custom("CustomeType".to_string())),
                Val::C('a'),
            ),
            (
                "let x: unit = 'a'",
                Id("x"),
                Some(ast::Type::Unit),
                Val::C('a'),
            ),
            (
                "let x: never = 'a'",
                Id("x"),
                Some(ast::Type::Never),
                Val::C('a'),
            ),
        ];

        for (input, id, ty, v) in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);
            test_let_by_stmt(&program.statements[0], &id, &ty, &v);
        }
    }

    #[test]
    fn test_type_by_function() {
        let inputs = vec![
            ("fn a() {};", (vec![], None)),
            ("fn a(x, y) {};", (vec![("x", None), ("y", None)], None)),
            (
                "fn a(x: int) {};",
                (vec![("x", Some(ast::Type::Integer))], None),
            ),
            (
                "fn a(x: int, y: char, z: string) {};",
                (
                    vec![
                        ("x", Some(ast::Type::Integer)),
                        ("y", Some(ast::Type::Char)),
                        ("z", Some(ast::Type::String)),
                    ],
                    None,
                ),
            ),
            (
                "fn a(x, y: int) {};",
                (vec![("x", None), ("y", Some(ast::Type::Integer))], None),
            ),
            ("fn a(): int {};", (vec![], Some(ast::Type::Integer))),
            (
                "fn a(x: int): int {};",
                (
                    vec![("x", Some(ast::Type::Integer))],
                    Some(ast::Type::Integer),
                ),
            ),
            (
                "fn a(x: int, y: char, z: string): int {};",
                (
                    vec![
                        ("x", Some(ast::Type::Integer)),
                        ("y", Some(ast::Type::Char)),
                        ("z", Some(ast::Type::String)),
                    ],
                    Some(ast::Type::Integer),
                ),
            ),
            (
                "fn a(x, y: char): int {};",
                (
                    vec![("x", None), ("y", Some(ast::Type::Char))],
                    Some(ast::Type::Integer),
                ),
            ),
        ];

        for (input, expected) in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);

            let stmt_expr = if let Stmt::ExprStmt(x) = &program.statements[0] {
                x
            } else {
                panic!("Expect type is Stmt::ExprStmt.");
            };

            let fn_expr = if let Expr::Function(x) = &stmt_expr.expr {
                x
            } else {
                panic!("Expect type is Expr::Function.");
            };

            assert_eq!(fn_expr.name, "a");

            assert_eq!(fn_expr.params.len(), expected.0.len());

            for (i, (id, ty)) in expected.0.into_iter().enumerate() {
                test_function_param(&fn_expr.params[i], id, &ty);
            }
            test_optional_type(&fn_expr.return_type, &expected.1);
        }
    }

    #[test]
    fn test_type_by_closure() {
        let inputs = vec![
            ("|| {};", (vec![], None)),
            ("|x, y| {};", (vec![("x", None), ("y", None)], None)),
            (
                "|x: int| {};",
                (vec![("x", Some(ast::Type::Integer))], None),
            ),
            (
                "|x: int, y: char, z: string| {};",
                (
                    vec![
                        ("x", Some(ast::Type::Integer)),
                        ("y", Some(ast::Type::Char)),
                        ("z", Some(ast::Type::String)),
                    ],
                    None,
                ),
            ),
            (
                "|x, y: int| {};",
                (vec![("x", None), ("y", Some(ast::Type::Integer))], None),
            ),
            ("||: int {};", (vec![], Some(ast::Type::Integer))),
            (
                "|x: int|: int {};",
                (
                    vec![("x", Some(ast::Type::Integer))],
                    Some(ast::Type::Integer),
                ),
            ),
            (
                "|x: int, y: char, z: string|: int {};",
                (
                    vec![
                        ("x", Some(ast::Type::Integer)),
                        ("y", Some(ast::Type::Char)),
                        ("z", Some(ast::Type::String)),
                    ],
                    Some(ast::Type::Integer),
                ),
            ),
            (
                "|x, y: char|: int {};",
                (
                    vec![("x", None), ("y", Some(ast::Type::Char))],
                    Some(ast::Type::Integer),
                ),
            ),
        ];

        for (input, expected) in inputs.into_iter() {
            let program = test_parse(input);
            assert_eq!(program.statements.len(), 1);

            let stmt_expr = if let Stmt::ExprStmt(x) = &program.statements[0] {
                x
            } else {
                panic!("Expect type is Stmt::ExprStmt.");
            };

            let closure = if let Expr::Closure(x) = &stmt_expr.expr {
                x
            } else {
                panic!("Expect type is Expr::Closure.");
            };

            assert_eq!(closure.params.len(), expected.0.len());

            for (i, (id, ty)) in expected.0.into_iter().enumerate() {
                test_closure_param(&closure.params[i], id, &ty);
            }
            test_optional_type(&closure.return_type, &expected.1);
        }
    }

    fn test_infix_by_expr(expr: &Expr, l: &Val, o: &str, r: &Val) {
        let infix = if let Expr::InfixExpr(x) = &expr {
            x
        } else {
            panic!("Expect type is Expr::InfixExpr.");
        };

        test_infix(infix, l, o, r);
    }

    fn test_infix_by_stmt(stmt: &Stmt, l: &Val, o: &str, r: &Val) {
        let expr_stmt = if let Stmt::ExprStmt(x) = stmt {
            x
        } else {
            panic!("Expect type is Stmt::ExprStmt");
        };

        let infix = if let Expr::InfixExpr(x) = &expr_stmt.expr {
            x
        } else {
            panic!("Expect type is Expr::InfixExpr");
        };

        test_infix(infix, l, o, r);
    }

    fn test_infix(infix: &ast::InfixExpr, l: &Val, o: &str, r: &Val) {
        test_expr(&infix.left, l);
        test_expr(&infix.right, r);
        test_operator(&infix.ope, o);
    }

    fn test_prefix_by_stmt(stmt: &Stmt, ope: &str, r: &Val) {
        let expr_stmt = if let Stmt::ExprStmt(x) = stmt {
            x
        } else {
            panic!("Expect type is Stmt::ExprStmt");
        };

        let prefix_expr = if let Expr::PrefixExpr(x) = &expr_stmt.expr {
            x
        } else {
            panic!("Expect type is Expr::PrefixExpr");
        };
        test_operator(&prefix_expr.ope, ope);
        test_expr(&prefix_expr.right, r);
    }

    fn test_operator(ope: &ast::Operator, ope_s: &str) {
        assert_eq!(
            match ope {
                ast::Operator::Assign => "=",
                ast::Operator::Asterisk => "*",
                ast::Operator::Bang => "!",
                ast::Operator::Equal => "==",
                ast::Operator::Gt => ">",
                ast::Operator::Lt => "<",
                ast::Operator::Minus => "-",
                ast::Operator::NotEqual => "!=",
                ast::Operator::Plus => "+",
                ast::Operator::Slash => "/",
            },
            ope_s,
        );
    }

    fn test_boolean_by_stmt(stmt: &Stmt, v: bool) {
        let expr_stmt = if let Stmt::ExprStmt(x) = stmt {
            x
        } else {
            panic!("Expect type is Stmt::ExprStmt");
        };

        let boolean_expr = if let Expr::Boolean(x) = &expr_stmt.expr {
            x
        } else {
            panic!("Expect type is Expr::Boolean");
        };
        assert_eq!(*boolean_expr, ast::Boolean { value: v });
    }

    fn test_char_by_stmt(stmt: &Stmt, v: char) {
        let expr_stmt = if let Stmt::ExprStmt(x) = stmt {
            x
        } else {
            panic!("Expect type is Stmt::ExprStmt");
        };

        let boolean_expr = if let Expr::Char(x) = &expr_stmt.expr {
            x
        } else {
            panic!("Expect type is Expr::Char");
        };
        assert_eq!(
            *boolean_expr,
            ast::Char {
                value: v.to_string()
            }
        );
    }

    fn test_let_by_stmt(stmt: &ast::Stmt, id: &Id, ty: &Option<ast::Type>, v: &Val) {
        let let_stmt = if let Stmt::Let(x) = stmt {
            x
        } else {
            panic!("Expect type is Stmt::Let");
        };

        test_identifier(&let_stmt.name, id.0);
        test_optional_type(&let_stmt.r#type, ty);
        test_expr(&let_stmt.value, v);
    }

    fn test_return_by_stmt(stmt: &Stmt, v: &Val) {
        let return_stmt = if let Stmt::Return(x) = stmt {
            x
        } else {
            panic!("Expect type is Stmt::Return");
        };

        test_expr(&return_stmt.return_value, v);
    }

    fn test_identifier_by_stmt(stmt: &Stmt, literal: &str) {
        let expr_stmt = if let Stmt::ExprStmt(x) = stmt {
            x
        } else {
            panic!("Expect type is Stmt::ExprStmt");
        };

        let identifier = if let Expr::Identifier(x) = &expr_stmt.expr {
            x
        } else {
            panic!("Expect type is Expr::Identifier");
        };
        test_identifier(identifier, literal);
    }

    fn test_interger_by_stmt(stmt: &Stmt, v: i64) {
        let expr_stmt = if let Stmt::ExprStmt(x) = stmt {
            x
        } else {
            panic!("Expect type is Stmt::ExprStmt");
        };

        let integer = if let Expr::Integer(x) = &expr_stmt.expr {
            x
        } else {
            panic!("Expect type is Expr::Integer");
        };
        test_integer(integer, v);
    }

    fn test_integer(integer: &ast::Integer, v: i64) {
        assert_eq!(*integer, ast::Integer { value: v });
    }

    fn test_identifier(identifier: &ast::Identifier, literal: &str) {
        assert_eq!(
            *identifier,
            ast::Identifier {
                value: literal.into()
            }
        );
    }

    fn test_function_param(param: &ast::FunctionParam, literal: &str, ty: &Option<ast::Type>) {
        assert_eq!(
            *param,
            ast::FunctionParam {
                name: ast::Identifier {
                    value: literal.into()
                },
                r#type: ty.to_owned()
            }
        );
    }

    fn test_closure_param(param: &ast::ClosureParam, literal: &str, ty: &Option<ast::Type>) {
        assert_eq!(
            *param,
            ast::ClosureParam {
                name: ast::Identifier {
                    value: literal.into()
                },
                r#type: ty.to_owned()
            }
        );
    }

    fn test_optional_type(actual: &Option<ast::Type>, expected: &Option<ast::Type>) {
        assert_eq!(actual, expected);
    }

    fn test_parse(input: &str) -> ast::Program {
        let lexer = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        parser.parse_program().unwrap()
    }

    fn test_expr(expr: &Expr, v: &Val) {
        match v {
            Val::S(v) => assert_eq!(
                *expr,
                Expr::StringLit(ast::StringLit {
                    value: v.to_string()
                })
            ),
            Val::B(v) => assert_eq!(*expr, Expr::Boolean(ast::Boolean { value: *v })),
            Val::Id(v) => assert_eq!(
                *expr,
                Expr::Identifier(ast::Identifier {
                    value: v.0.to_string(),
                })
            ),
            Val::I(v) => assert_eq!(*expr, Expr::Integer(ast::Integer { value: *v })),
            Val::Infix(l, o, r) => test_infix_by_expr(expr, l, o, r),
            Val::C(v) => assert_eq!(
                *expr,
                Expr::Char(ast::Char {
                    value: v.to_string()
                })
            ),
        }
    }

    fn test_errors(tests: Vec<(&str, ParserError)>) {
        tests.into_iter().for_each(|(input, expected)| {
            let lexer = lexer::Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let result = parser.parse_program();
            let err = result.unwrap_err();
            assert_eq!(err.downcast::<ParserError>().unwrap(), expected);
        });
    }
}
