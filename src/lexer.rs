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
            None => Token::eof_with(self.position(start_pos, 1)),
            Some(c) => match c {
                '=' => match self.peek_char() {
                    Some('=') => {
                        self.read_char();
                        Token::equal_with(self.position(start_pos, 1))
                    }
                    _ => Token::assign_with(self.position(start_pos, 1)),
                },
                '+' => Token::plus_with(self.position(start_pos, 1)),
                '-' => Token::minus_with(self.position(start_pos, 1)),
                '!' => match self.peek_char() {
                    Some('=') => {
                        self.read_char();
                        Token::not_equal_with(self.position(start_pos, 1))
                    }
                    _ => Token::bang_with(self.position(start_pos, 1)),
                },
                '*' => Token::asterisk_with(self.position(start_pos, 1)),
                '/' => Token::slash_with(self.position(start_pos, 1)),
                '<' => Token::lt_with(self.position(start_pos, 1)),
                '>' => Token::gt_with(self.position(start_pos, 1)),
                ',' => Token::comma_with(self.position(start_pos, 1)),
                ';' => Token::semicolon_with(self.position(start_pos, 1)),
                ':' => Token::colon_with(self.position(start_pos, 1)),
                '(' => Token::lparen_with(self.position(start_pos, 1)),
                ')' => Token::rparen_with(self.position(start_pos, 1)),
                '{' => Token::lbrace_with(self.position(start_pos, 1)),
                '}' => Token::rbrace_with(self.position(start_pos, 1)),
                '[' => Token::lbracket_with(self.position(start_pos, 1)),
                ']' => Token::rbracket_with(self.position(start_pos, 1)),
                '|' => Token::vertical_bar_with(self.position(start_pos, 1)),
                '"' => Token::string_literal_with(self.read_string(), self.position(start_pos, 1)),
                '\'' => Token::char_with(self.read_native_char(), self.position(start_pos, 1)),
                _ => {
                    if Self::is_letter(c) {
                        let literal = self.read_identifier();
                        return lookup_ident(&literal, self.position(start_pos, 0));
                    } else if Self::is_digit(c) {
                        return Token::int_with(self.read_number(), self.position(start_pos, 0));
                    } else {
                        Token::illegal_with(c.to_string(), self.position(start_pos, 1))
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

    fn position(&self, start_pos: usize, offset: usize) -> Position {
        Position {
            range: start_pos..self.pos + offset,
        }
    }

    fn is_letter(ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_' || ch == '!' || ch == '?'
    }

    fn is_digit(ch: char) -> bool {
        ch.is_ascii_digit()
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
        ";

        let mut lexer = super::Lexer::new(input.to_string());

        let mut assert_token = |token: Token, offset: usize| {
            let tkn = lexer.next_token();
            let literal = tkn.literal();
            assert_eq!(tkn, token);
            if let Token::Eof(_) = tkn {
                assert_eq!(tkn.position().range, input.len()..input.len() + 1);
                assert_eq!(offset, input.len());
                return;
            }

            assert_eq!(offset..offset + literal.len(), tkn.position().range);
            assert_eq!(&input[tkn.position().range.clone()], literal);
        };

        {
            assert_token(Token::r#let(), 13);
            assert_token(Token::ident_with("five".into(), Position::default()), 17);
            assert_token(Token::assign(), 22);
            assert_token(Token::int_with("5".into(), Position::default()), 24);
            assert_token(Token::semicolon(), 25);
        }

        {
            assert_token(Token::r#let(), 39);
            assert_token(Token::ident_with("ten".into(), Position::default()), 43);
            assert_token(Token::assign(), 47);
            assert_token(Token::int_with("10".into(), Position::default()), 49);
            assert_token(Token::semicolon(), 51);
        }

        {
            assert_token(Token::r#let(), 65);
            assert_token(
                Token::ident_with("with_type".into(), Position::default()),
                69,
            );
            assert_token(Token::colon(), 78);
            assert_token(Token::ident_with("String".into(), Position::default()), 80);
            assert_token(Token::assign(), 87);
            assert_token(
                Token::string_literal_with("aaa".into(), Position::default()),
                89,
            );
            assert_token(Token::semicolon(), 94);
        }

        {
            assert_token(Token::function(), 109);
            assert_token(Token::ident_with("add".into(), Position::default()), 112);
            assert_token(Token::lparen(), 115);
            assert_token(Token::ident_with("x".into(), Position::default()), 116);
            assert_token(Token::comma(), 117);
            assert_token(Token::ident_with("y".into(), Position::default()), 119);
            assert_token(Token::rparen(), 120);
            assert_token(Token::lbrace(), 122);
            assert_token(Token::ident_with("x".into(), Position::default()), 140);
            assert_token(Token::plus(), 142);
            assert_token(Token::ident_with("y".into(), Position::default()), 144);
            assert_token(Token::semicolon(), 145);
            assert_token(Token::rbrace(), 159);
        }

        {
            assert_token(Token::function(), 174);
            assert_token(Token::ident_with("add".into(), Position::default()), 177);
            assert_token(Token::lparen(), 180);
            assert_token(Token::ident_with("x".into(), Position::default()), 181);
            assert_token(Token::comma(), 182);
            assert_token(Token::ident_with("y".into(), Position::default()), 184);
            assert_token(Token::rparen(), 185);
            assert_token(Token::lbrace(), 187);
            assert_token(Token::ident_with("x".into(), Position::default()), 205);
            assert_token(Token::plus(), 207);
            assert_token(Token::ident_with("y".into(), Position::default()), 209);
            assert_token(Token::semicolon(), 210);
            assert_token(Token::rbrace(), 224);
            assert_token(Token::semicolon(), 225);
        }

        {
            assert_token(Token::r#let(), 240);
            assert_token(Token::ident_with("add".into(), Position::default()), 244);
            assert_token(Token::assign(), 248);
            assert_token(Token::vertical_bar(), 250);
            assert_token(Token::ident_with("x".into(), Position::default()), 251);
            assert_token(Token::comma(), 252);
            assert_token(Token::ident_with("y".into(), Position::default()), 254);
            assert_token(Token::vertical_bar(), 255);
            assert_token(Token::lbrace(), 257);
            assert_token(Token::ident_with("x".into(), Position::default()), 275);
            assert_token(Token::plus(), 277);
            assert_token(Token::ident_with("y".into(), Position::default()), 279);
            assert_token(Token::semicolon(), 280);
            assert_token(Token::rbrace(), 294);
            assert_token(Token::semicolon(), 295);
        }

        {
            assert_token(Token::r#let(), 310);
            assert_token(Token::ident_with("result".into(), Position::default()), 314);
            assert_token(Token::assign(), 321);
            assert_token(Token::ident_with("add".into(), Position::default()), 323);
            assert_token(Token::lparen(), 326);
            assert_token(Token::ident_with("five".into(), Position::default()), 327);
            assert_token(Token::comma(), 331);
            assert_token(Token::ident_with("ten".into(), Position::default()), 333);
            assert_token(Token::rparen(), 336);
            assert_token(Token::semicolon(), 337);
        }

        {
            assert_token(Token::bang(), 351);
            assert_token(Token::minus(), 352);
            assert_token(Token::slash(), 353);
            assert_token(Token::asterisk(), 354);
            assert_token(Token::int_with("5".into(), Position::default()), 355);
            assert_token(Token::semicolon(), 356);
        }

        {
            assert_token(Token::int_with("5".into(), Position::default()), 370);
            assert_token(Token::lt(), 372);
            assert_token(Token::int_with("10".into(), Position::default()), 374);
            assert_token(Token::gt(), 377);
            assert_token(Token::int_with("5".into(), Position::default()), 379);
            assert_token(Token::semicolon(), 380);
        }

        {
            assert_token(Token::r#if(), 395);
            assert_token(Token::lparen(), 397);
            assert_token(Token::int_with("5".into(), Position::default()), 398);
            assert_token(Token::lt(), 400);
            assert_token(Token::int_with("10".into(), Position::default()), 402);
            assert_token(Token::rparen(), 404);
            assert_token(Token::lbrace(), 406);
            assert_token(Token::r#return(), 424);
            assert_token(Token::r#true(), 431);
            assert_token(Token::semicolon(), 435);
            assert_token(Token::rbrace(), 449);

            assert_token(Token::r#else(), 451);
            assert_token(Token::lbrace(), 456);
            assert_token(Token::r#return(), 474);
            assert_token(Token::r#false(), 481);
            assert_token(Token::semicolon(), 486);
            assert_token(Token::rbrace(), 500);
        }

        {
            assert_token(Token::r#if(), 515);
            assert_token(Token::int_with("5".into(), Position::default()), 518);
            assert_token(Token::lt(), 520);
            assert_token(Token::int_with("10".into(), Position::default()), 522);
            assert_token(Token::lbrace(), 525);
            assert_token(Token::r#return(), 543);
            assert_token(Token::r#true(), 550);
            assert_token(Token::semicolon(), 554);
            assert_token(Token::rbrace(), 568);
            assert_token(Token::r#else(), 570);
            assert_token(Token::lbrace(), 575);
            assert_token(Token::r#return(), 593);
            assert_token(Token::r#false(), 600);
            assert_token(Token::semicolon(), 605);
            assert_token(Token::rbrace(), 619);
        }

        {
            assert_token(Token::int_with("10".into(), Position::default()), 634);
            assert_token(Token::equal(), 637);
            assert_token(Token::int_with("10".into(), Position::default()), 640);
            assert_token(Token::semicolon(), 642);
        }

        {
            assert_token(Token::int_with("10".into(), Position::default()), 656);
            assert_token(Token::not_equal(), 659);
            assert_token(Token::int_with("9".into(), Position::default()), 662);
            assert_token(Token::semicolon(), 663);
        }

        {
            assert_token(
                Token::string_literal_with("foobar".into(), Position::default()),
                677,
            );
        }

        {
            assert_token(
                Token::string_literal_with("foo bar".into(), Position::default()),
                698,
            );
        }

        {
            assert_token(Token::char_with("a".into(), Position::default()), 720);
        }

        {
            assert_token(Token::lbracket(), 736);
            assert_token(Token::int_with("1".into(), Position::default()), 737);
            assert_token(Token::comma(), 738);
            assert_token(Token::int_with("2".into(), Position::default()), 740);
            assert_token(Token::rbracket(), 741);
            assert_token(Token::semicolon(), 742);
        }

        {
            assert_token(Token::lbrace(), 756);
            assert_token(
                Token::string_literal_with("foo".into(), Position::default()),
                757,
            );
            assert_token(Token::colon(), 762);
            assert_token(
                Token::string_literal_with("bar".into(), Position::default()),
                764,
            );
            assert_token(Token::rbrace(), 769);
        }

        {
            assert_token(Token::eof(), 779);
        }
    }
}
