use std::collections::HashSet;

use ecow::EcoString;

use crate::parser::TextRange;
use crate::token_kind::TokenKind;
use crate::token_stream::TokenStream;
use crate::T;

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

    fn peek(&self) -> TokenKind {
        self.token_stream.peek()
    }

    fn peek_range(&self) -> TextRange {
        self.token_stream.peek_range()
    }

    fn peek_text(&self) -> &str {
        self.token_stream.peek_text()
    }

    fn take_error(&mut self) -> Option<EcoString> {
        self.token_stream.take_error()
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

    pub fn define_macro(&mut self, macro_name: impl Into<EcoString>) {
        self.macros.insert(macro_name.into());
    }

    pub fn macros(&self) -> &HashSet<EcoString> {
        &self.macros
    }

    fn next_token(&mut self) -> TokenKind {
        match self.token_stream.eat() {
            // T![#ifdef] => self.process_if(IfKind::Defined),
            // T![#ifndef] => self.process_if(IfKind::NotDefined),
            T![#define] => self.process_define(),
            kind => kind,
        }
    }

    fn error(&mut self, message: impl Into<EcoString>) -> TokenKind {
        self.error = Some(message.into());
        TokenKind::Error
    }

    fn process_if(&mut self, kind: IfKind) {
        let kind = self.token_stream.eat();
        match kind {
            TokenKind::Id => {}
            _ => {}
        }
    }

    fn process_define(&mut self) -> TokenKind {
        self.eat_trivia();
        match self.token_stream.peek() {
            TokenKind::Id => {
                let macro_name = self.token_stream.peek_text().to_owned();
                self.define_macro(macro_name);

                self.token_stream.eat(); // eat Id
                self.eat_trivia();
                self.next_token()
            }
            _ => self.error("expected macro name after #define"),
        }
    }

    fn eat_trivia(&mut self) {
        while self.token_stream.peek().is_trivia() {
            self.token_stream.eat();
        }
    }
}

enum IfKind {
    Defined,
    NotDefined,
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::preprocessor::PreProcessor;
    use crate::token_kind::TokenKind;
    use crate::token_stream::TokenStream;

    #[test]
    fn define_1() {
        let lexer = Lexer::new("#define FOO\n");
        let mut prep = PreProcessor::new(lexer);
        assert!(!prep.macros().contains("FOO"));
        assert_eq!(prep.eat(), TokenKind::Eof); // eat define
        assert!(prep.macros().contains("FOO"));
    }

    #[test]
    fn define_2() {
        let lexer = Lexer::new("text1\n#define FOO\ntext2");
        let mut prep = PreProcessor::new(lexer);
        assert!(!prep.macros().contains("FOO"));
        assert_eq!(prep.eat(), TokenKind::Id);
        assert_eq!(prep.eat(), TokenKind::Whitespace);
        assert_eq!(prep.eat(), TokenKind::Id); // eat define
        assert!(prep.macros().contains("FOO"));
        assert_eq!(prep.eat(), TokenKind::Eof);
    }
}
