use ecow::{eco_format, EcoString};

use crate::{
    kind::{SyntaxKind, TokenKind},
    lexer::Lexer,
    node::SyntaxNode,
};

#[derive(Debug)]
pub(crate) struct Parser<'a> {
    text: &'a str,
    lexer: Lexer<'a>,
    current: TokenKind,
    current_start: usize,
    nodes: Vec<SyntaxNode>,
}

#[derive(Debug)]
pub(crate) struct Marker(usize);

impl<'a> Parser<'a> {
    pub(crate) fn new(text: &'a str) -> Self {
        let mut lexer = Lexer::new(text);
        let current = lexer.next();
        Self {
            text,
            lexer,
            current,
            current_start: 0,
            nodes: vec![],
        }
    }

    pub(crate) fn finish(self) -> Vec<SyntaxNode> {
        self.nodes
    }

    pub(crate) fn current(&self) -> TokenKind {
        self.current
    }

    pub(crate) fn current_start(&self) -> usize {
        self.current_start
    }

    pub(crate) fn current_text(&self) -> &'a str {
        let from = self.current_start;
        let to = self.lexer.cursor();
        &self.text[from..to]
    }

    pub(crate) fn marker(&self) -> Marker {
        Marker(self.nodes.len())
    }

    pub(crate) fn wrap(&mut self, from: Marker, kind: SyntaxKind) {
        let from = from.0;
        let to = self.nodes.len();
        let children = self.nodes.drain(from..to).collect();
        self.nodes.insert(from, SyntaxNode::node(kind, children));
    }

    pub(crate) fn at(&self, kind: TokenKind) -> bool {
        self.current == kind
    }

    pub(crate) fn eof(&self) -> bool {
        self.at(TokenKind::Eof)
    }

    pub(crate) fn error(&mut self, message: impl Into<EcoString>) {
        self.nodes
            .push(SyntaxNode::error(message, self.current_text()))
    }

    pub(crate) fn error_and_eat(&mut self, message: impl Into<EcoString>) {
        let m = self.marker();
        self.eat();
        self.nodes[m.0] = SyntaxNode::error(message, self.current_text());
    }

    pub(crate) fn assert(&mut self, kind: TokenKind) {
        assert!(self.eat_if(kind));
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) -> bool {
        if self.eat_if(kind) {
            return true;
        }
        self.error(eco_format!("expected {kind:?}"));
        false
    }

    pub(crate) fn eat(&mut self) {
        self.consume_token();
        while self.current.is_trivia() {
            self.consume_token();
        }
    }

    pub(crate) fn consume_token(&mut self) {
        let text = self.current_text();
        if self.at(TokenKind::Error) {
            let message = self.lexer.take_error().unwrap();
            self.nodes.push(SyntaxNode::error(message, text));
        } else {
            self.nodes.push(SyntaxNode::token(self.current, text));
        }

        self.current_start = self.lexer.cursor();
        self.current = self.lexer.next();
    }

    pub(crate) fn eat_if(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.eat();
            true
        } else {
            false
        }
    }
}
