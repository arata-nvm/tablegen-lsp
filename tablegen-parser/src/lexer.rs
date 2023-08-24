use ecow::EcoString;
use unscanny::Scanner;

use crate::{token::TokenKind, T};

#[derive(Debug)]
pub struct Lexer<'a> {
    s: Scanner<'a>,
    error: Option<EcoString>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            s: Scanner::new(text),
            error: None,
        }
    }

    pub fn take_error(&mut self) -> Option<EcoString> {
        self.error.take()
    }

    fn error(&mut self, msg: impl Into<EcoString>) -> TokenKind {
        self.error = Some(msg.into());
        TokenKind::Error
    }

    pub fn next(&mut self) -> TokenKind {
        match self.s.eat() {
            Some(c) if c.is_ascii_digit() => self.number(),
            Some(c) if is_identifier_start(c) => self.identifier(),
            Some('-') => T![-],
            Some('+') => T![+],
            Some('[') => T!['['],
            Some(']') => T![']'],
            Some('{') => T!['{'],
            Some('}') => T!['}'],
            Some('(') => T!['('],
            Some(')') => T![')'],
            Some('<') => T![<],
            Some('>') => T![>],
            Some(':') => T![:],
            Some(';') => T![;],
            Some(',') => T![,],
            Some('=') => T![=],
            Some('?') => T![?],
            Some('#') => T![#],
            Some('.') => {
                if self.s.eat_if('.') {
                    if self.s.eat_if('.') {
                        T![...]
                    } else {
                        self.error("Invalid '..' punctuation")
                    }
                } else {
                    T![.]
                }
            }
            None => TokenKind::Eof,
            _ => unimplemented!(),
        }
    }

    fn number(&mut self) -> TokenKind {
        self.s.eat_while(char::is_ascii_digit);
        TokenKind::IntVal
    }

    fn identifier(&mut self) -> TokenKind {
        self.s.eat_while(is_identifier_continue);
        TokenKind::Id
    }
}

fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_identifier_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use crate::token::TokenKind;

    use super::Lexer;

    fn tokenize(text: &str) -> Vec<TokenKind> {
        let mut l = Lexer::new(text);

        let mut tokens = Vec::new();
        while tokens.last() != Some(&TokenKind::Eof) {
            tokens.push(l.next());
        }
        tokens
    }

    #[test]
    fn symbol() {
        insta::assert_debug_snapshot!(tokenize("-+[]{}()<>:;,.=?#..."));
    }

    #[test]
    fn error() {
        let mut l = Lexer::new("..");
        assert_eq!(l.next(), TokenKind::Error);
        assert_eq!(l.take_error(), Some("Invalid '..' punctuation".into()));
    }

    #[test]
    fn number() {
        insta::assert_debug_snapshot!(tokenize("42"));
    }

    #[test]
    fn string() {
        insta::assert_debug_snapshot!(tokenize("hoge"))
    }
}
