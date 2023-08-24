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
            Some(c) if c.is_ascii_digit() => self.number(),
            Some(c) if is_identifier_start(c) => self.identifier(start),
            Some('!') => self.bangoperator(),
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

    fn whitespace(&mut self) -> TokenKind {
        self.s.eat_while(char::is_ascii_whitespace);
        TokenKind::Whitespace
    }

    fn number(&mut self) -> TokenKind {
        self.s.eat_while(char::is_ascii_digit);
        TokenKind::IntVal
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
    fn number() {
        insta::assert_debug_snapshot!(tokenize("42"));
    }

    #[test]
    fn string() {
        insta::assert_debug_snapshot!(tokenize("hoge"))
    }
}
