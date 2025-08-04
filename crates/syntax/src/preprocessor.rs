use std::collections::HashSet;
use std::ops::Range;

use ecow::EcoString;

use crate::T;
use crate::token_kind::TokenKind;
use crate::token_stream::TokenStream;

#[derive(Debug)]
pub struct PreProcessor<T: TokenStream> {
    token_stream: T,
    macros: HashSet<EcoString>,
    error: Option<EcoString>,
}

impl<T: TokenStream> TokenStream for PreProcessor<T> {
    fn eat(&mut self) -> TokenKind {
        self.next_token()
    }

    fn cursor(&self) -> usize {
        self.token_stream.cursor()
    }

    fn text(&self, range: Range<usize>) -> &str {
        self.token_stream.text(range)
    }

    fn take_error(&mut self) -> Option<EcoString> {
        if self.error.is_some() {
            self.error.take()
        } else {
            self.token_stream.take_error()
        }
    }
}

impl<T: TokenStream> PreProcessor<T> {
    pub fn new(token_stream: T) -> Self {
        Self {
            token_stream,
            macros: HashSet::new(),
            error: None,
        }
    }

    pub fn define_macro(&mut self, macro_name: EcoString) {
        self.macros.insert(macro_name);
    }

    pub fn macros(&self) -> &HashSet<EcoString> {
        &self.macros
    }

    fn next_token(&mut self) -> TokenKind {
        match self.token_stream.eat() {
            T![#ifdef] => self.process_if(IfKind::Defined),
            T![#ifndef] => self.process_if(IfKind::NotDefined),
            T![#else] => self.process_else(),
            T![#endif] => self.process_endif(),
            T![#define] => self.process_define(),
            kind => kind,
        }
    }

    fn error(&mut self, message: impl Into<EcoString>) -> TokenKind {
        self.error = Some(message.into());
        TokenKind::Error
    }

    fn process_if(&mut self, if_kind: IfKind) -> TokenKind {
        match self.next_not_trivia() {
            (start, TokenKind::Id) => {
                let end = self.token_stream.cursor();
                let macro_name = self.token_stream.text(start..end);
                let macro_defined = self.macros.contains(macro_name);

                if let (IfKind::Defined, false) | (IfKind::NotDefined, true) =
                    (if_kind, macro_defined)
                {
                    self.eat_until_else_or_endif();
                }
                TokenKind::PreProcessor
            }
            _ => match if_kind {
                IfKind::Defined => self.error("expected macro name after #ifdef"),
                IfKind::NotDefined => self.error("expected macro name after #ifndef"),
            },
        }
    }

    fn process_else(&mut self) -> TokenKind {
        self.eat_until_else_or_endif();
        TokenKind::PreProcessor
    }

    fn process_endif(&mut self) -> TokenKind {
        TokenKind::PreProcessor
    }

    fn process_define(&mut self) -> TokenKind {
        match self.next_not_trivia() {
            (start, TokenKind::Id) => {
                let end = self.token_stream.cursor();
                let macro_name = self.token_stream.text(start..end);
                self.define_macro(macro_name.into());
                TokenKind::PreProcessor
            }
            _ => self.error("expected macro name after #define"),
        }
    }

    fn next_not_trivia(&mut self) -> (usize, TokenKind) {
        loop {
            let start = self.token_stream.cursor();
            let kind = self.token_stream.eat();
            if !kind.is_trivia() {
                return (start, kind);
            }
        }
    }

    fn eat_until_else_or_endif(&mut self) {
        let mut depth = 1;
        loop {
            match self.token_stream.eat() {
                T![#ifdef] | T![#ifndef] => {
                    depth += 1;
                }
                T![#endif] if depth >= 2 => {
                    depth -= 1;
                }
                T![#else] | T![#endif] if depth == 1 => {
                    break;
                }
                TokenKind::Eof => {
                    self.error("reached EOF without matching #endif");
                    break;
                }
                _ => {}
            }
        }
    }
}

enum IfKind {
    Defined,
    NotDefined,
}

#[cfg(test)]
mod tests {
    use ecow::EcoString;

    use crate::T;
    use crate::lexer::Lexer;
    use crate::preprocessor::PreProcessor;
    use crate::token_kind::TokenKind;
    use crate::token_stream::TokenStream;

    #[test]
    fn define_1() {
        let lexer = Lexer::new("#define FOO\n");
        let mut prep = PreProcessor::new(lexer);
        assert!(!prep.macros().contains("FOO"));
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat define
        assert!(prep.macros().contains("FOO"));
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::Eof);
    }

    #[test]
    fn define_2() {
        let lexer = Lexer::new("text1\n#define FOO\ntext2");
        let mut prep = PreProcessor::new(lexer);
        assert!(!prep.macros().contains("FOO"));
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat define
        assert!(prep.macros().contains("FOO"));
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Eof);
    }

    #[test]
    fn ifdef_defined() {
        let lexer = Lexer::new("text1\n#ifdef HOGE\ntext2\n#endif\ntext3");
        let mut prep = PreProcessor::new(lexer);
        prep.define_macro(EcoString::from("HOGE"));
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat ifdef
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat endif
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Eof);
    }

    #[test]
    fn ifdef_not_defined() {
        let lexer = Lexer::new("text1\n#ifdef HOGE\ntext2\n#endif\ntext3");
        let mut prep = PreProcessor::new(lexer);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat ifdef - endif
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Eof);
    }

    #[test]
    fn ifndef_defined() {
        let lexer = Lexer::new("text1\n#ifndef HOGE\ntext2\n#endif\ntext3");
        let mut prep = PreProcessor::new(lexer);
        prep.define_macro(EcoString::from("HOGE"));
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat ifdef - endif
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Eof);
    }

    #[test]
    fn ifndef_not_defined() {
        let lexer = Lexer::new("text1\n#ifndef HOGE\ntext2\n#endif\ntext3");
        let mut prep = PreProcessor::new(lexer);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat ifdef
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat endif
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Eof);
    }

    #[test]
    fn ifdef_nested() {
        let lexer = Lexer::new(
            r"text1
            #ifdef HOGE
            text2
            #ifdef FUGA
            text3
            #endif
            text4
            #endif
            text5",
        );
        let mut prep = PreProcessor::new(lexer);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat ifdef - endif
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Eof);
    }

    #[test]
    fn ifdef_else_not_defined() {
        let text = r"text1
        #ifdef HOGE
        [
        #else
        ]
        #endif
        text2";

        let mut prep = PreProcessor::new(Lexer::new(text));
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat ifdef - else
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), T![']']);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat endif
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Eof);
    }

    #[test]
    fn ifdef_else_defined() {
        let text = r"text1
        #ifdef HOGE
        [
        #else
        ]
        #endif
        text2";

        let mut prep = PreProcessor::new(Lexer::new(text));
        prep.define_macro(EcoString::from("HOGE"));
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat ifdef
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), T!['[']);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::PreProcessor); // eat else - endif
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Eof);
    }
}
