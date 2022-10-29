use std::ops::Range;

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct PureToken {
    pub position: Position,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct TokenWithInput {
    pub input: String,
    pub position: Position,
}

#[derive(Clone, Debug)]
pub enum Token {
    Illegal(TokenWithInput),
    Eof(PureToken),
    // identifier, literal
    Ident(TokenWithInput),
    Int(TokenWithInput),
    StringLiteral(TokenWithInput),
    Char(TokenWithInput),
    // operator
    Assign(PureToken),
    Plus(PureToken),
    Minus(PureToken),
    Bang(PureToken),
    Asterisk(PureToken),
    Slash(PureToken),
    Equal(PureToken),
    NotEqual(PureToken),
    Lt(PureToken),
    Gt(PureToken),
    // delimiter
    Comma(PureToken),
    Semicolon(PureToken),
    Colon(PureToken),
    Lparen(PureToken),
    Rparen(PureToken),
    Lbrace(PureToken),
    Rbrace(PureToken),
    Lbracket(PureToken),
    Rbracket(PureToken),
    VerticalBar(PureToken),
    // keyword
    Function(PureToken),
    Let(PureToken),
    True(PureToken),
    False(PureToken),
    If(PureToken),
    Else(PureToken),
    Return(PureToken),
}

impl<'a> Token {
    pub fn literal(&self) -> String {
        match self {
            Self::Assign(_) => "=".to_string(),
            Self::Plus(_) => "+".to_string(),
            Self::Minus(_) => "-".to_string(),
            Self::Bang(_) => "!".to_string(),
            Self::Asterisk(_) => "*".to_string(),
            Self::Slash(_) => "/".to_string(),
            Self::Equal(_) => "==".to_string(),
            Self::NotEqual(_) => "!=".to_string(),
            Self::Lt(_) => "<".to_string(),
            Self::Gt(_) => ">".to_string(),
            Self::Comma(_) => ",".to_string(),
            Self::Semicolon(_) => ";".to_string(),
            Self::Colon(_) => ":".to_string(),
            Self::Lparen(_) => "(".to_string(),
            Self::Rparen(_) => ")".to_string(),
            Self::Lbrace(_) => "{".to_string(),
            Self::Rbrace(_) => "}".to_string(),
            Self::Lbracket(_) => "[".to_string(),
            Self::Rbracket(_) => "]".to_string(),
            Self::VerticalBar(_) => "|".to_string(),
            Self::Function(_) => "fn".to_string(),
            Self::Let(_) => "let".to_string(),
            Self::True(_) => "true".to_string(),
            Self::False(_) => "false".to_string(),
            Self::If(_) => "if".to_string(),
            Self::Else(_) => "else".to_string(),
            Self::Return(_) => "return".to_string(),
            Self::Eof(_) => "".to_string(),
            Self::Illegal(t) | Self::Ident(t) | Self::Int(t) => t.input.to_string(),
            Self::StringLiteral(t) => format!("\"{}\"", t.input),
            Self::Char(t) => format!("'{}'", &t.input),
        }
    }

    pub fn position(&'a self) -> &'a Position {
        match self {
            Self::Assign(t)
            | Self::Plus(t)
            | Self::Minus(t)
            | Self::Bang(t)
            | Self::Asterisk(t)
            | Self::Slash(t)
            | Self::Equal(t)
            | Self::NotEqual(t)
            | Self::Lt(t)
            | Self::Gt(t)
            | Self::Comma(t)
            | Self::Semicolon(t)
            | Self::Colon(t)
            | Self::Lparen(t)
            | Self::Rparen(t)
            | Self::Lbrace(t)
            | Self::Rbrace(t)
            | Self::Lbracket(t)
            | Self::Rbracket(t)
            | Self::VerticalBar(t)
            | Self::Function(t)
            | Self::Let(t)
            | Self::True(t)
            | Self::False(t)
            | Self::If(t)
            | Self::Else(t)
            | Self::Return(t)
            | Self::Eof(t) => &t.position,
            Self::Illegal(t)
            | Self::Ident(t)
            | Self::Int(t)
            | Self::StringLiteral(t)
            | Self::Char(t) => &t.position,
        }
    }
}

impl Token {
    pub fn illegal() -> Self {
        Token::Illegal(Default::default())
    }

    pub fn illegal_with(input: String, position: Position) -> Self {
        Token::Illegal(TokenWithInput { input, position })
    }

    pub fn eof() -> Self {
        Token::Eof(Default::default())
    }

    pub fn eof_with(position: impl Into<Position>) -> Self {
        Token::Eof(PureToken {
            position: position.into(),
        })
    }

    pub fn ident() -> Self {
        Token::Ident(Default::default())
    }

    pub fn ident_with(input: String, position: impl Into<Position>) -> Self {
        Token::Ident(TokenWithInput {
            input,
            position: position.into(),
        })
    }

    pub fn int() -> Self {
        Token::Int(Default::default())
    }

    pub fn int_with(input: String, position: impl Into<Position>) -> Self {
        Token::Int(TokenWithInput {
            input,
            position: position.into(),
        })
    }

    pub fn string_literal() -> Self {
        Token::StringLiteral(Default::default())
    }

    pub fn string_literal_with(input: String, position: impl Into<Position>) -> Self {
        Token::StringLiteral(TokenWithInput {
            input,
            position: position.into(),
        })
    }

    pub fn char() -> Self {
        Token::Char(Default::default())
    }

    pub fn char_with(input: String, position: impl Into<Position>) -> Self {
        Token::Char(TokenWithInput {
            input,
            position: position.into(),
        })
    }

    pub fn assign() -> Self {
        Token::Assign(Default::default())
    }

    pub fn assign_with(position: impl Into<Position>) -> Self {
        Token::Assign(PureToken {
            position: position.into(),
        })
    }

    pub fn plus() -> Self {
        Token::Plus(Default::default())
    }

    pub fn plus_with(position: impl Into<Position>) -> Self {
        Token::Plus(PureToken {
            position: position.into(),
        })
    }

    pub fn minus() -> Self {
        Token::Minus(Default::default())
    }

    pub fn minus_with(position: impl Into<Position>) -> Self {
        Token::Minus(PureToken {
            position: position.into(),
        })
    }

    pub fn bang() -> Self {
        Token::Bang(Default::default())
    }

    pub fn bang_with(position: impl Into<Position>) -> Self {
        Token::Bang(PureToken {
            position: position.into(),
        })
    }

    pub fn asterisk() -> Self {
        Token::Asterisk(Default::default())
    }

    pub fn asterisk_with(position: impl Into<Position>) -> Self {
        Token::Asterisk(PureToken {
            position: position.into(),
        })
    }

    pub fn slash() -> Self {
        Token::Slash(Default::default())
    }

    pub fn slash_with(position: impl Into<Position>) -> Self {
        Token::Slash(PureToken {
            position: position.into(),
        })
    }

    pub fn equal() -> Self {
        Token::Equal(Default::default())
    }

    pub fn equal_with(position: impl Into<Position>) -> Self {
        Token::Equal(PureToken {
            position: position.into(),
        })
    }

    pub fn not_equal() -> Self {
        Token::NotEqual(Default::default())
    }

    pub fn not_equal_with(position: impl Into<Position>) -> Self {
        Token::NotEqual(PureToken {
            position: position.into(),
        })
    }

    pub fn lt() -> Self {
        Token::Lt(Default::default())
    }

    pub fn lt_with(position: impl Into<Position>) -> Self {
        Token::Lt(PureToken {
            position: position.into(),
        })
    }

    pub fn gt() -> Self {
        Token::Gt(Default::default())
    }

    pub fn gt_with(position: impl Into<Position>) -> Self {
        Token::Gt(PureToken {
            position: position.into(),
        })
    }

    pub fn comma() -> Self {
        Token::Comma(Default::default())
    }

    pub fn comma_with(position: impl Into<Position>) -> Self {
        Token::Comma(PureToken {
            position: position.into(),
        })
    }

    pub fn semicolon() -> Self {
        Token::Semicolon(Default::default())
    }

    pub fn semicolon_with(position: impl Into<Position>) -> Self {
        Token::Semicolon(PureToken {
            position: position.into(),
        })
    }

    pub fn colon() -> Self {
        Token::Colon(Default::default())
    }

    pub fn colon_with(position: impl Into<Position>) -> Self {
        Token::Colon(PureToken {
            position: position.into(),
        })
    }

    pub fn lparen() -> Self {
        Token::Lparen(Default::default())
    }

    pub fn lparen_with(position: impl Into<Position>) -> Self {
        Token::Lparen(PureToken {
            position: position.into(),
        })
    }

    pub fn rparen() -> Self {
        Token::Rparen(Default::default())
    }

    pub fn rparen_with(position: impl Into<Position>) -> Self {
        Token::Rparen(PureToken {
            position: position.into(),
        })
    }

    pub fn lbrace() -> Self {
        Token::Lbrace(Default::default())
    }

    pub fn lbrace_with(position: impl Into<Position>) -> Self {
        Token::Lbrace(PureToken {
            position: position.into(),
        })
    }

    pub fn rbrace() -> Self {
        Token::Rbrace(Default::default())
    }

    pub fn rbrace_with(position: impl Into<Position>) -> Self {
        Token::Rbrace(PureToken {
            position: position.into(),
        })
    }

    pub fn lbracket() -> Self {
        Token::Lbracket(Default::default())
    }

    pub fn lbracket_with(position: impl Into<Position>) -> Self {
        Token::Lbracket(PureToken {
            position: position.into(),
        })
    }

    pub fn rbracket() -> Self {
        Token::Rbracket(Default::default())
    }

    pub fn rbracket_with(position: impl Into<Position>) -> Self {
        Token::Rbracket(PureToken {
            position: position.into(),
        })
    }

    pub fn vertical_bar() -> Self {
        Token::VerticalBar(Default::default())
    }

    pub fn vertical_bar_with(position: impl Into<Position>) -> Self {
        Token::VerticalBar(PureToken {
            position: position.into(),
        })
    }

    pub fn function() -> Self {
        Token::Function(Default::default())
    }

    pub fn function_with(position: impl Into<Position>) -> Self {
        Token::Function(PureToken {
            position: position.into(),
        })
    }

    pub fn r#let() -> Self {
        Token::Let(Default::default())
    }

    pub fn let_with(position: impl Into<Position>) -> Self {
        Token::Let(PureToken {
            position: position.into(),
        })
    }

    pub fn r#true() -> Self {
        Token::True(Default::default())
    }

    pub fn true_with(position: impl Into<Position>) -> Self {
        Token::True(PureToken {
            position: position.into(),
        })
    }

    pub fn r#false() -> Self {
        Token::False(Default::default())
    }

    pub fn false_with(position: impl Into<Position>) -> Self {
        Token::False(PureToken {
            position: position.into(),
        })
    }

    pub fn r#if() -> Self {
        Token::If(Default::default())
    }

    pub fn if_with(position: impl Into<Position>) -> Self {
        Token::If(PureToken {
            position: position.into(),
        })
    }

    pub fn r#else() -> Self {
        Token::Else(Default::default())
    }

    pub fn else_with(position: impl Into<Position>) -> Self {
        Token::Else(PureToken {
            position: position.into(),
        })
    }

    pub fn r#return() -> Self {
        Token::Return(Default::default())
    }

    pub fn return_with(position: impl Into<Position>) -> Self {
        Token::Return(PureToken {
            position: position.into(),
        })
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        match (self, other) {
            (Token::Illegal(l), Token::Illegal(r)) => l.input == r.input,
            (Token::Eof(_), Token::Eof(_)) => true,
            (Token::Ident(l), Token::Ident(r)) => l.input == r.input,
            (Token::Int(l), Token::Int(r)) => l.input == r.input,
            (Token::StringLiteral(l), Token::StringLiteral(r)) => l.input == r.input,
            (Token::Char(l), Token::Char(r)) => l.input == r.input,
            (Token::Assign(_), Token::Assign(_)) => true,
            (Token::Plus(_), Token::Plus(_)) => true,
            (Token::Minus(_), Token::Minus(_)) => true,
            (Token::Bang(_), Token::Bang(_)) => true,
            (Token::Asterisk(_), Token::Asterisk(_)) => true,
            (Token::Slash(_), Token::Slash(_)) => true,
            (Token::Equal(_), Token::Equal(_)) => true,
            (Token::NotEqual(_), Token::NotEqual(_)) => true,
            (Token::Lt(_), Token::Lt(_)) => true,
            (Token::Gt(_), Token::Gt(_)) => true,
            (Token::Comma(_), Token::Comma(_)) => true,
            (Token::Semicolon(_), Token::Semicolon(_)) => true,
            (Token::Colon(_), Token::Colon(_)) => true,
            (Token::Lparen(_), Token::Lparen(_)) => true,
            (Token::Rparen(_), Token::Rparen(_)) => true,
            (Token::Lbrace(_), Token::Lbrace(_)) => true,
            (Token::Rbrace(_), Token::Rbrace(_)) => true,
            (Token::Lbracket(_), Token::Lbracket(_)) => true,
            (Token::Rbracket(_), Token::Rbracket(_)) => true,
            (Token::VerticalBar(_), Token::VerticalBar(_)) => true,
            (Token::Function(_), Token::Function(_)) => true,
            (Token::Let(_), Token::Let(_)) => true,
            (Token::True(_), Token::True(_)) => true,
            (Token::False(_), Token::False(_)) => true,
            (Token::If(_), Token::If(_)) => true,
            (Token::Else(_), Token::Else(_)) => true,
            (Token::Return(_), Token::Return(_)) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct Position {
    pub range: Range<usize>,
}
