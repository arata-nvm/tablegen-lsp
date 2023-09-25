use drop_bomb::DropBomb;
use ecow::{eco_format, EcoString};

use crate::{
    error::{Position, Range},
    kind::{SyntaxKind, TokenKind},
    lexer::Lexer,
    node::SyntaxNode,
};

#[derive(Debug)]
pub(crate) struct Marker {
    pos: usize,
    bomb: DropBomb,
}

impl Marker {
    pub fn new(pos: usize) -> Self {
        Self {
            pos,
            bomb: DropBomb::new("Marker must be explicitly completed"),
        }
    }

    pub fn complete(mut self) -> CompletedMarker {
        self.bomb.defuse();
        CompletedMarker::success()
    }

    pub fn abandon(mut self) -> CompletedMarker {
        self.bomb.defuse();
        CompletedMarker::fail()
    }
}

#[derive(Debug)]
pub struct CompletedMarker {
    success: bool,
}

impl CompletedMarker {
    pub(crate) fn success() -> Self {
        Self { success: true }
    }

    pub(crate) fn fail() -> Self {
        Self { success: false }
    }

    pub(crate) fn is_success(&self) -> bool {
        self.success
    }

    pub(crate) fn or_error(self, parser: &mut Parser, message: impl Into<EcoString>) {
        if !self.success {
            parser.error(message);
        }
    }
}

#[derive(Debug)]
pub(crate) struct Parser<'a> {
    text: &'a str,
    lexer: Lexer<'a>,
    current: TokenKind,
    current_start: Position,
    current_end: Position,
    nodes: Vec<SyntaxNode>,
    recover_tokens: &'a [TokenKind],
}

impl<'a> Parser<'a> {
    pub(crate) fn new(text: &'a str, recover_tokens: &'a [TokenKind]) -> Self {
        let mut lexer = Lexer::new(text);
        let current = lexer.next();
        let current_end = lexer.cursor();
        Self {
            text,
            lexer,
            current,
            current_start: 0,
            current_end,
            nodes: vec![],
            recover_tokens,
        }
    }

    pub(crate) fn finish(self) -> Vec<SyntaxNode> {
        self.nodes
    }

    pub(crate) fn current(&self) -> TokenKind {
        self.current
    }

    fn current_range(&self) -> Range {
        self.current_start..self.current_end
    }

    fn current_text(&self) -> &'a str {
        &self.text[self.current_range()]
    }

    pub(crate) fn marker(&self) -> Marker {
        Marker::new(self.nodes.len())
    }

    pub(crate) fn abandon(&self, m: Marker) -> CompletedMarker {
        m.abandon()
    }

    pub(crate) fn wrap(&mut self, from: Marker, kind: SyntaxKind) -> CompletedMarker {
        self.wrap_range(from, self.cursor_before_trivia(), kind)
    }

    pub(crate) fn wrap_all(&mut self, from: Marker, kind: SyntaxKind) -> CompletedMarker {
        self.wrap_range(from, self.nodes.len(), kind)
    }

    fn wrap_range(&mut self, from_marker: Marker, to: usize, kind: SyntaxKind) -> CompletedMarker {
        let from = from_marker.pos;
        let to = to.max(from);
        let children = self.nodes.drain(from..to).collect();
        self.nodes.insert(from, SyntaxNode::node(kind, children));
        from_marker.complete()
    }

    fn cursor_before_trivia(&self) -> usize {
        let mut cursor = self.nodes.len();
        while cursor > 0 && self.nodes[cursor - 1].token_kind().is_trivia() {
            cursor -= 1;
        }
        cursor
    }

    pub(crate) fn at(&self, kind: TokenKind) -> bool {
        self.current == kind
    }

    pub(crate) fn at_set(&self, set: &[TokenKind]) -> bool {
        set.contains(&self.current)
    }

    pub(crate) fn eof(&self) -> bool {
        self.at(TokenKind::Eof)
    }

    pub(crate) fn error(&mut self, message: impl Into<EcoString>) {
        self.nodes.push(SyntaxNode::error(
            self.current_range(),
            message,
            self.current_text(),
        ))
    }

    pub(crate) fn error_and_eat(&mut self, message: impl Into<EcoString>) {
        self.error(message);

        let m = self.marker();
        self.eat();
        self.wrap(m, SyntaxKind::Error);
    }

    pub(crate) fn error_and_recover(&mut self, message: impl Into<EcoString>) {
        self.error(message);

        if self.at_set(self.recover_tokens) {
            let m = self.marker();
            self.eat();
            self.wrap(m, SyntaxKind::Error);
        }
    }

    pub(crate) fn assert(&mut self, kind: TokenKind) {
        assert!(self.eat_if(kind));
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) {
        self.expect_with_msg(kind, eco_format!("expected {kind:?}"))
    }

    pub(crate) fn expect_with_msg(&mut self, kind: TokenKind, message: impl Into<EcoString>) {
        if self.eat_if(kind) {
            return;
        }

        if !self.after_error() {
            self.error(message);
        }
    }

    pub(crate) fn after_error(&self) -> bool {
        let cursor = self.cursor_before_trivia();
        self.nodes[cursor - 1].kind().is_error()
    }

    pub(crate) fn eat_if(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.eat();
            true
        } else {
            false
        }
    }

    pub(crate) fn eat(&mut self) {
        self.consume_token();
        self.eat_trivia();
    }

    pub(crate) fn consume_token(&mut self) {
        if self.at(TokenKind::Error) {
            let message = self.lexer.take_error().unwrap();
            self.error(message);
        } else {
            self.nodes.push(SyntaxNode::token(
                self.current,
                self.current_text(),
                self.current_range(),
            ));
        }

        self.current_start = self.lexer.cursor();
        self.current = self.lexer.next();
        self.current_end = self.lexer.cursor();
    }

    pub(crate) fn eat_trivia(&mut self) {
        while self.current.is_trivia() {
            self.consume_token();
        }
    }
}
