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
        let start = self.s.cursor();
        match self.s.eat() {
            Some(c) if c.is_whitespace() => self.whitespace(),
            Some(c) if c.is_ascii_digit() => self.number(start, c),
            Some(c) if is_identifier_start(c) => self.identifier(start),
            Some('!') => self.bangoperator(),
            Some('-') => self.number(start, '-'),
            Some('+') => self.number(start, '+'),
            Some('"') => self.string(),
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
            Some('#') => self.preprocessor(start),
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

    fn whitespace(&mut self) -> TokenKind {
        self.s.eat_while(char::is_ascii_whitespace);
        TokenKind::Whitespace
    }

    fn number(&mut self, mut start: usize, c: char) -> TokenKind {
        match self.s.peek() {
            Some(c2) if !c2.is_ascii_digit() => match c {
                '+' => return TokenKind::Plus,
                '-' => return TokenKind::Minus,
                _ => {}
            },
            _ => {}
        }

        let mut base = 10;
        if c == '0' {
            if self.s.eat_if('b') {
                base = 2;
            } else if self.s.eat_if('x') {
                base = 16;
            }

            if base != 10 {
                start = self.s.cursor();
            }
        }

        match base {
            2 => self.s.eat_while(|c| matches!(c, '0' | '1')),
            10 => self.s.eat_while(char::is_ascii_digit),
            16 => self.s.eat_while(char::is_ascii_hexdigit),
            _ => unreachable!(),
        };

        let number = self.s.get(start..self.s.cursor());
        if let Err(_) = i64::from_str_radix(number, base) {
            match base {
                2 => return self.error("Invalid number"),
                10 => return self.error("Invalid binary number"),
                16 => return self.error("Invalid hexadecimal number"),
                _ => unreachable!(),
            }
        }

        match base {
            2 => TokenKind::BinaryIntVal,
            10 | 16 => TokenKind::IntVal,
            _ => unreachable!(),
        }
    }

    fn string(&mut self) -> TokenKind {
        loop {
            match self.s.eat() {
                Some('"') => break,
                Some('\r') | Some('\n') => return self.error("End of line in string literal"),
                None => return self.error("End of file in string literal"),
                _ => {}
            }
        }

        TokenKind::StrVal
    }

    fn identifier(&mut self, start: usize) -> TokenKind {
        self.s.eat_while(is_identifier_continue);
        let ident = self.s.from(start);

        match ident {
            "assert" => T![assert],
            "bit" => T![bit],
            "bits" => T![bits],
            "class" => T![class],
            "code" => T![code],
            "dag" => T![dag],
            "def" => T![def],
            "defm" => T![defm],
            "defset" => T![defset],
            "defvar" => T![defvar],
            "else" => T![else],
            "field" => T![field],
            "foreach" => T![foreach],
            "if" => T![if],
            "in" => T![in],
            "include" => T![include],
            "int" => T![int],
            "let" => T![let],
            "list" => T![list],
            "multiclass" => T![multiclass],
            "string" => T![string],
            "then" => T![then],
            "true" => T![true],
            "false" => T![false],
            _ => TokenKind::Id,
        }
    }

    fn bangoperator(&mut self) -> TokenKind {
        let start = self.s.cursor();
        self.s.eat_while(char::is_ascii_alphabetic);
        let ident = self.s.from(start);

        match ident {
            "concat" => T![!concat],
            "add" => T![!add],
            "sub" => T![!sub],
            "mul" => T![!mul],
            "div" => T![!div],
            "not" => T![!not],
            "log2" => T![!log2],
            "and" => T![!and],
            "or" => T![!or],
            "xor" => T![!xor],
            "sra" => T![!sra],
            "srl" => T![!srl],
            "shl" => T![!shl],
            "listconcat" => T![!listconcat],
            "listsplat" => T![!listsplat],
            "strconcat" => T![!strconcat],
            "interleave" => T![!interleave],
            "substr" => T![!substr],
            "find" => T![!find],
            "cast" => T![!cast],
            "subst" => T![!subst],
            "foreach" => T![!foreach],
            "filter" => T![!filter],
            "foldl" => T![!foldl],
            "head" => T![!head],
            "tail" => T![!tail],
            "size" => T![!size],
            "empty" => T![!empty],
            "if" => T![!if],
            "cond" => T![!cond],
            "eq" => T![!eq],
            "isa" => T![!isa],
            "dag" => T![!dag],
            "ne" => T![!ne],
            "le" => T![!le],
            "lt" => T![!lt],
            "ge" => T![!ge],
            "gt" => T![!gt],
            "setdagop" => T![!setdagop],
            "getdagop" => T![!getdagop],
            "exists" => T![!exists],
            "listremove" => T![!listremove],
            "tolower" => T![!tolower],
            "toupper" => T![!toupper],
            "range" => T![!range],
            "getdagarg" => T![!getdagarg],
            "getdagname" => T![!getdagname],
            "setdagarg" => T![!setdagarg],
            "setdagname" => T![!setdagname],
            _ => self.error("Unknown operator"),
        }
    }

    fn preprocessor(&mut self, start: usize) -> TokenKind {
        match self.s.peek() {
            Some(c) if c.is_alphabetic() => {}
            _ => return T![#],
        }

        self.s.eat_while(char::is_alphabetic);
        let ident = self.s.from(start);

        match ident {
            "#ifdef" => T![#ifdef],
            "#ifndef" => T![#ifndef],
            "#else" => T![#else],
            "#endif" => T![#endif],
            "#define" => T![#define],
            _ => self.error("Unknown preprocessor directive"),
        }
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

        let mut l = Lexer::new("!hoge");
        assert_eq!(l.next(), TokenKind::Error);
        assert_eq!(l.take_error(), Some("Unknown operator".into()));
    }

    #[test]
    fn keyword() {
        insta::assert_debug_snapshot!(tokenize(
            "assert bit bits class code dag def defm defset defvar else field foreach if in include int let list multiclass string then"
        ));
    }

    #[test]
    fn bangoperator() {
        insta::assert_debug_snapshot!(tokenize(
          "!concat !add !sub !mul !div !not !log2 !and !or !xor !sra !srl !shl !listconcat !listsplat !strconcat !interleave !substr !find !cast !subst !foreach !filter !foldl !head !tail !size !empty !if !cond !eq !isa !dag !ne !le !lt !ge !gt !setdagop !getdagop !exists !listremove !tolower !toupper !range !getdagarg !getdagname !setdagarg !setdagname"
        ));
    }

    #[test]
    fn literal() {
        insta::assert_debug_snapshot!(tokenize("true false 42 +42 -42 0xff 0b01"));
    }

    #[test]
    fn string() {
        insta::assert_debug_snapshot!(tokenize("hoge \"fuga\""))
    }

    #[test]
    fn preprocessor() {
        insta::assert_debug_snapshot!(tokenize("#ifdef #ifndef #else #endif #define"))
    }
}
