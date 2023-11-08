use std::collections::HashMap;

use crate::{kind::TokenKind, lexer::Lexer, T};

#[derive(Debug)]
pub struct PreProcessor<'a> {
    lexer: Lexer<'a>,
    macros: HashMap<String, String>,
}

impl<'a> PreProcessor<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            macros: HashMap::new(),
        }
    }

    pub fn next(&mut self) -> TokenKind {
        let kind = self.lexer.next();
        match kind {
            T![#ifdef] => self.process_if(IfKind::Defined),
            T![#ifndef] => self.process_if(IfKind::NotDefined),
            T![#define] => self.process_define(),
            _ => {}
        }
        kind
    }

    fn process_if(&mut self, kind: IfKind) {
        let kind = self.lexer.next();
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
