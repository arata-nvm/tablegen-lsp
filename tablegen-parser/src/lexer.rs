use crate::{parser::Position, token_kind::TokenKind, T};
use ecow::EcoString;
use unscanny::Scanner;

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

    pub fn cursor(&self) -> Position {
        self.s.cursor()
    }

    fn error(&mut self, msg: impl Into<EcoString>) -> TokenKind {
        self.error = Some(msg.into());
        TokenKind::Error
    }

    pub fn next(&mut self) -> TokenKind {
        let start = self.s.cursor();
        match self.s.eat() {
            Some(c) if c.is_whitespace() => self.whitespace(),
            Some('/') if self.s.eat_if('/') => self.line_comment(),
            Some('/') if self.s.eat_if('*') => self.block_comment(),

            Some(c) if c.is_ascii_digit() => self.number(start, c),
            Some('-') => self.number(start, '-'),
            Some('+') => self.number(start, '+'),

            Some(c) if is_identifier_start(c) => self.identifier(start),
            Some('"') => self.string(),
            Some('$') => self.var_name(),
            Some('[') if self.s.eat_if('{') => self.code_fragment(),

            Some('!') => self.bangoperator(),
            Some('#') => self.preprocessor(start),

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
            Some('.') if self.s.eat_if('.') => {
                if self.s.eat_if('.') {
                    T![...]
                } else {
                    self.error("Invalid '..' punctuation")
                }
            }
            Some('.') => T![.],
            None => TokenKind::Eof,
            _ => self.error("Unexpected character"),
        }
    }

    fn whitespace(&mut self) -> TokenKind {
        self.s.eat_while(char::is_ascii_whitespace);
        TokenKind::Whitespace
    }

    fn line_comment(&mut self) -> TokenKind {
        self.s.eat_until(is_newline);
        TokenKind::LineComment
    }

    fn block_comment(&mut self) -> TokenKind {
        self.s.eat_until("*/");
        self.s.eat_if("*/");
        TokenKind::BlockComment
    }

    fn number(&mut self, mut start: Position, c: char) -> TokenKind {
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
        if i64::from_str_radix(number, base).is_err() {
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

    fn identifier(&mut self, start: Position) -> TokenKind {
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

    fn string(&mut self) -> TokenKind {
        let mut escaped = false;
        loop {
            match self.s.eat() {
                Some('\\') => escaped = true,
                Some('"') if !escaped => break,
                Some('\r') | Some('\n') => return self.error("End of line in string literal"),
                None => return self.error("End of file in string literal"),
                _ => escaped = false,
            }
        }

        TokenKind::StrVal
    }

    fn var_name(&mut self) -> TokenKind {
        if !self.s.eat_if(is_identifier_start) {
            return self.error("Invalid variable name");
        }
        self.s.eat_while(is_identifier_continue);
        TokenKind::VarName
    }

    fn code_fragment(&mut self) -> TokenKind {
        self.s.eat_until("}]");
        if self.s.eat_if("}]") {
            TokenKind::CodeFragment
        } else {
            self.error("Unterminated code block")
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

    fn preprocessor(&mut self, start: Position) -> TokenKind {
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

fn is_newline(c: char) -> bool {
    matches!(c, '\r' | '\n')
}

#[cfg(test)]
mod tests {
    use crate::token_kind::TokenKind;

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
    fn line_comment() {
        insta::assert_debug_snapshot!(tokenize("// hogefuga\n42"));
    }

    #[test]
    fn block_comment() {
        insta::assert_debug_snapshot!(tokenize("/* hogefuga */42"));
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
        insta::assert_debug_snapshot!(tokenize(r#"hoge "fuga\n\"" $piyo [{ foo }]"#))
    }

    #[test]
    fn preprocessor() {
        insta::assert_debug_snapshot!(tokenize("#ifdef #ifndef #else #endif #define"))
    }
}
