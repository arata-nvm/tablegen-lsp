use std::collections::HashMap;

use ecow::EcoString;

use crate::parser::TextRange;
use crate::token_stream::TokenStream;
use crate::{token_kind::TokenKind, T};

#[derive(Debug)]
pub struct PreProcessor<T: TokenStream> {
    token_stream: T,
    macros: HashMap<String, String>,
}

impl<T: TokenStream> TokenStream for PreProcessor<T> {
    fn eat(&mut self) -> TokenKind {
        self.token_stream.eat()
    }

    fn peek(&self) -> TokenKind {
        self.token_stream.peek()
    }

    fn take_error(&mut self) -> Option<EcoString> {
        self.token_stream.take_error()
    }

    fn current_range(&self) -> TextRange {
        self.token_stream.current_range()
    }

    fn current_text(&self) -> &str {
        self.token_stream.current_text()
    }
}

impl<T: TokenStream> PreProcessor<T> {
    pub fn new(token_stream: T) -> Self {
        Self {
            token_stream,
            macros: HashMap::new(),
        }
    }

    pub fn next(&mut self) -> TokenKind {
        let kind = self.token_stream.eat();
        match kind {
            T![#ifdef] => self.process_if(IfKind::Defined),
            T![#ifndef] => self.process_if(IfKind::NotDefined),
            T![#define] => self.process_define(),
            _ => {}
        }
        kind
    }

    fn process_if(&mut self, kind: IfKind) {
        let kind = self.token_stream.eat();
        match kind {
            TokenKind::Id => {}
            _ => {}
        }
    }

    fn process_define(&mut self) {}
}

enum IfKind {
    Defined,
    NotDefined,
}
