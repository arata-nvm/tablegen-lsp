use unscanny::Scanner;

use crate::token::TokenKind;

#[derive(Debug)]
pub struct Lexer<'a> {
    s: Scanner<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            s: Scanner::new(text),
        }
    }

    pub fn next(&mut self) -> TokenKind {
        match self.s.eat() {
            Some(c) if c.is_ascii_digit() => self.number(),
            None => TokenKind::Eof,
            _ => unimplemented!(),
        }
    }

    fn number(&mut self) -> TokenKind {
        self.s.eat_while(char::is_ascii_digit);
        TokenKind::IntVal
    }
}
