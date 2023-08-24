use unscanny::Scanner;

use crate::{token::TokenKind, T};

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
            Some('.') => T![.],
            Some('=') => T![=],
            Some('?') => T![?],
            Some('#') => T![#],
            None => TokenKind::Eof,
            _ => unimplemented!(),
        }
    }

    fn number(&mut self) -> TokenKind {
        self.s.eat_while(char::is_ascii_digit);
        TokenKind::IntVal
    }
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
        insta::assert_debug_snapshot!(tokenize("-+[]{}()<>:;,.=?#"));
    }

    #[test]
    fn number() {
        insta::assert_debug_snapshot!(tokenize("42"));
    }
}
