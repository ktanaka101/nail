use crate::ast_parser;
use crate::token::{Position, Token};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    pos: usize,
    read_pos: usize,
    ch: Option<char>,
}

impl ast_parser::Lexer for Lexer {
    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let start_pos = self.pos;
        let token = match self.ch {
            None => Token::eof_with(self.position(start_pos)),
            Some(c) => match c {
                '=' => match self.peek_char() {
                    Some('=') => {
                        self.read_char();
                        Token::equal_with(self.position(start_pos))
                    }
                    _ => Token::assign_with(self.position(start_pos)),
                },
                '+' => Token::plus_with(self.position(start_pos)),
                '-' => Token::minus_with(self.position(start_pos)),
                '!' => match self.peek_char() {
                    Some('=') => {
                        self.read_char();
                        Token::not_equal_with(self.position(start_pos))
                    }
                    _ => Token::bang_with(self.position(start_pos)),
                },
                '*' => Token::asterisk_with(self.position(start_pos)),
                '/' => Token::slash_with(self.position(start_pos)),
                '<' => Token::lt_with(self.position(start_pos)),
                '>' => Token::gt_with(self.position(start_pos)),
                ',' => Token::comma_with(self.position(start_pos)),
                ';' => Token::semicolon_with(self.position(start_pos)),
                ':' => Token::colon_with(self.position(start_pos)),
                '(' => Token::lparen_with(self.position(start_pos)),
                ')' => Token::rparen_with(self.position(start_pos)),
                '{' => Token::lbrace_with(self.position(start_pos)),
                '}' => Token::rbrace_with(self.position(start_pos)),
                '[' => Token::lbracket_with(self.position(start_pos)),
                ']' => Token::rbracket_with(self.position(start_pos)),
                '|' => Token::vertical_bar_with(self.position(start_pos)),
                '"' => Token::string_literal_with(self.read_string(), self.position(start_pos)),
                '\'' => Token::char_with(self.read_native_char(), self.position(start_pos)),
                _ => {
                    if Self::is_letter(c) {
                        let literal = self.read_identifier();
                        return lookup_ident(&literal, self.position(start_pos));
                    } else if Self::is_digit(c) {
                        return Token::int_with(self.read_number(), self.position(start_pos));
                    } else {
                        Token::illegal_with(c.to_string(), self.position(start_pos))
                    }
                }
            },
        };

        self.read_char();
        token
    }
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input,
            pos: 0,
            read_pos: 0,
            ch: None,
        };

        lexer.read_char();
        lexer
    }

    fn position(&self, start_pos: usize) -> Position {
        Position {
            range: start_pos..self.pos + 1,
        }
    }

    fn is_letter(ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_' || ch == '!' || ch == '?'
    }

    fn is_digit(ch: char) -> bool {
        ch.is_digit(10)
    }

    fn read_char(&mut self) {
        self.ch = self.peek_char();
        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_pos >= self.input.chars().count() {
            None
        } else {
            self.input.chars().nth(self.read_pos)
        }
    }

    fn read_range(&self, s: usize, e: usize) -> String {
        self.input.chars().skip(s).take(e - s).collect::<String>()
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.pos;
        while let Some(c) = self.ch {
            if Self::is_letter(c) {
                self.read_char();
            } else {
                break;
            }
        }

        self.read_range(pos, self.pos)
    }

    fn read_number(&mut self) -> String {
        let pos = self.pos;
        while let Some(c) = self.ch {
            if Self::is_digit(c) {
                self.read_char();
            } else {
                break;
            }
        }

        self.read_range(pos, self.pos)
    }

    fn read_string(&mut self) -> String {
        let pos = self.pos + 1;
        loop {
            self.read_char();
            match self.ch {
                Some('"') => break,
                None => break,
                _ => (),
            }
        }
        self.read_range(pos, self.pos)
    }

    fn read_native_char(&mut self) -> String {
        let pos = self.pos + 1;
        loop {
            self.read_char();
            match self.ch {
                Some('\'') => break,
                None => break,
                _ => (),
            }
        }
        self.read_range(pos, self.pos)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.ch {
            if c.is_ascii_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }
}

fn lookup_ident(ident: &str, position: Position) -> Token {
    match ident {
        "fn" => Token::function_with(position),
        "let" => Token::let_with(position),
        "true" => Token::true_with(position),
        "false" => Token::false_with(position),
        "if" => Token::if_with(position),
        "else" => Token::else_with(position),
        "return" => Token::return_with(position),
        "macro" => Token::macro_with(position),
        id => Token::ident_with(id.into(), position),
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast_parser::Lexer, token::TokenWithInput};

    use super::*;

    #[test]
    fn it_return_token() {
        assert_eq!(
            lookup_ident("fn", Default::default()),
            Token::Function(Default::default())
        );
        assert_eq!(
            lookup_ident("let", Default::default()),
            Token::Let(Default::default())
        );
        assert_eq!(
            lookup_ident("true", Default::default()),
            Token::True(Default::default())
        );
        assert_eq!(
            lookup_ident("false", Default::default()),
            Token::False(Default::default())
        );
        assert_eq!(
            lookup_ident("if", Default::default()),
            Token::If(Default::default())
        );
        assert_eq!(
            lookup_ident("else", Default::default()),
            Token::Else(Default::default())
        );
        assert_eq!(
            lookup_ident("return", Default::default()),
            Token::Return(Default::default())
        );
        assert_eq!(
            lookup_ident("fna", Default::default()),
            Token::Ident(TokenWithInput {
                input: "fna".into(),
                position: Default::default()
            })
        );
    }

    #[test]
    fn return_tokens() {
        let input = "
            let five = 5;
            let ten = 10;
            let with_type: String = \"aaa\";

            fn add(x, y) {
                x + y;
            }

            fn add(x, y) {
                x + y;
            };

            let add = |x, y| {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if(5 < 10) {
                return true;
            } else {
                return false;
            }

            if 5 < 10 {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            \"foobar\"
            \"foo bar\"
            'a'
            [1, 2];
            {\"foo\": \"bar\"}
            macro(x, y) { x + y; };
        ";

        let mut lexer = super::Lexer::new(input.to_string());

        assert_eq!(lexer.next_token(), Token::r#let());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("five".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::assign());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("5".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::semicolon());

        assert_eq!(lexer.next_token(), Token::r#let());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("ten".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::assign());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("10".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::semicolon());

        assert_eq!(lexer.next_token(), Token::r#let());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("with_type".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::colon());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("String".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::assign());
        assert_eq!(
            lexer.next_token(),
            Token::string_literal_with("aaa".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::semicolon());

        assert_eq!(lexer.next_token(), Token::function());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("add".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::lparen());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("x".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::comma());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("y".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::rparen());
        assert_eq!(lexer.next_token(), Token::lbrace());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("x".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::plus());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("y".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::semicolon());
        assert_eq!(lexer.next_token(), Token::rbrace());

        assert_eq!(lexer.next_token(), Token::function());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("add".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::lparen());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("x".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::comma());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("y".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::rparen());
        assert_eq!(lexer.next_token(), Token::lbrace());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("x".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::plus());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("y".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::semicolon());
        assert_eq!(lexer.next_token(), Token::rbrace());
        assert_eq!(lexer.next_token(), Token::semicolon());

        assert_eq!(lexer.next_token(), Token::r#let());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("add".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::assign());
        assert_eq!(lexer.next_token(), Token::vertical_bar());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("x".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::comma());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("y".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::vertical_bar());
        assert_eq!(lexer.next_token(), Token::lbrace());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("x".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::plus());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("y".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::semicolon());
        assert_eq!(lexer.next_token(), Token::rbrace());
        assert_eq!(lexer.next_token(), Token::semicolon());

        assert_eq!(lexer.next_token(), Token::r#let());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("result".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::assign());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("add".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::lparen());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("five".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::comma());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("ten".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::rparen());
        assert_eq!(lexer.next_token(), Token::semicolon());

        assert_eq!(lexer.next_token(), Token::bang());
        assert_eq!(lexer.next_token(), Token::minus());
        assert_eq!(lexer.next_token(), Token::slash());
        assert_eq!(lexer.next_token(), Token::asterisk());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("5".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::semicolon());

        assert_eq!(
            lexer.next_token(),
            Token::int_with("5".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::lt());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("10".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::gt());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("5".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::semicolon());

        assert_eq!(lexer.next_token(), Token::r#if());
        assert_eq!(lexer.next_token(), Token::lparen());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("5".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::lt());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("10".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::rparen());
        assert_eq!(lexer.next_token(), Token::lbrace());
        assert_eq!(lexer.next_token(), Token::r#return());
        assert_eq!(lexer.next_token(), Token::r#true());
        assert_eq!(lexer.next_token(), Token::semicolon());
        assert_eq!(lexer.next_token(), Token::rbrace());
        assert_eq!(lexer.next_token(), Token::r#else());
        assert_eq!(lexer.next_token(), Token::lbrace());
        assert_eq!(lexer.next_token(), Token::r#return());
        assert_eq!(lexer.next_token(), Token::r#false());
        assert_eq!(lexer.next_token(), Token::semicolon());
        assert_eq!(lexer.next_token(), Token::rbrace());

        assert_eq!(lexer.next_token(), Token::r#if());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("5".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::lt());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("10".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::lbrace());
        assert_eq!(lexer.next_token(), Token::r#return());
        assert_eq!(lexer.next_token(), Token::r#true());
        assert_eq!(lexer.next_token(), Token::semicolon());
        assert_eq!(lexer.next_token(), Token::rbrace());
        assert_eq!(lexer.next_token(), Token::r#else());
        assert_eq!(lexer.next_token(), Token::lbrace());
        assert_eq!(lexer.next_token(), Token::r#return());
        assert_eq!(lexer.next_token(), Token::r#false());
        assert_eq!(lexer.next_token(), Token::semicolon());
        assert_eq!(lexer.next_token(), Token::rbrace());

        assert_eq!(
            lexer.next_token(),
            Token::int_with("10".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::equal());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("10".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::semicolon());

        assert_eq!(
            lexer.next_token(),
            Token::int_with("10".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::not_equal());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("9".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::semicolon());

        assert_eq!(
            lexer.next_token(),
            Token::string_literal_with("foobar".into(), Position::default())
        );

        assert_eq!(
            lexer.next_token(),
            Token::string_literal_with("foo bar".into(), Position::default())
        );

        assert_eq!(
            lexer.next_token(),
            Token::char_with("a".into(), Position::default())
        );

        assert_eq!(lexer.next_token(), Token::lbracket());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("1".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::comma());
        assert_eq!(
            lexer.next_token(),
            Token::int_with("2".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::rbracket());
        assert_eq!(lexer.next_token(), Token::semicolon());

        assert_eq!(lexer.next_token(), Token::lbrace());
        assert_eq!(
            lexer.next_token(),
            Token::string_literal_with("foo".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::colon());
        assert_eq!(
            lexer.next_token(),
            Token::string_literal_with("bar".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::rbrace());

        assert_eq!(lexer.next_token(), Token::r#macro());
        assert_eq!(lexer.next_token(), Token::lparen());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("x".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::comma());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("y".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::rparen());
        assert_eq!(lexer.next_token(), Token::lbrace());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("x".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::plus());
        assert_eq!(
            lexer.next_token(),
            Token::ident_with("y".into(), Position::default())
        );
        assert_eq!(lexer.next_token(), Token::semicolon());
        assert_eq!(lexer.next_token(), Token::rbrace());
        assert_eq!(lexer.next_token(), Token::semicolon());
        assert_eq!(lexer.next_token(), Token::eof());
    }
}
