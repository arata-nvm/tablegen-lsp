use ecow::{eco_format, EcoString};
use rowan::GreenNodeBuilder;
pub use rowan::{TextRange, TextSize};

use crate::{
    error::TableGenError, language::SyntaxNode, lexer::Lexer, syntax_kind::SyntaxKind,
    token_kind::TokenKind,
};

#[derive(Debug)]
pub(crate) enum CompletedMarker {
    Success,
    Fail,
}

impl CompletedMarker {
    pub(crate) fn is_success(&self) -> bool {
        matches!(self, Self::Success)
    }

    pub(crate) fn or_error(self, parser: &mut Parser, message: impl Into<EcoString>) {
        if !self.is_success() {
            parser.error(message);
        }
    }
}

#[derive(Debug)]
pub(crate) struct Parser<'a> {
    text: &'a str,
    recover_tokens: &'a [TokenKind],

    lexer: Lexer<'a>,
    builder: GreenNodeBuilder<'static>,

    errors: Vec<TableGenError>,
    is_after_error: bool,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(text: &'a str, recover_tokens: &'a [TokenKind]) -> Self {
        Self {
            text,
            recover_tokens,

            lexer: Lexer::new(text),
            builder: GreenNodeBuilder::new(),

            errors: Vec::new(),
            is_after_error: false,
        }
    }

    #[inline]
    pub(crate) fn finish(self) -> (SyntaxNode, Vec<TableGenError>) {
        (SyntaxNode::new_root(self.builder.finish()), self.errors)
    }

    #[inline]
    pub(crate) fn builder(&mut self) -> &mut GreenNodeBuilder<'static> {
        &mut self.builder
    }

    #[inline]
    pub(crate) fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(kind.into());
    }

    #[inline]
    pub(crate) fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    #[inline]
    pub(crate) fn current(&self) -> TokenKind {
        self.lexer.current()
    }

    #[inline]
    fn current_range(&self) -> TextRange {
        self.lexer.current_range()
    }

    #[inline]
    fn current_text(&self) -> &'a str {
        self.lexer.current_text()
    }

    #[inline]
    pub(crate) fn at(&self, kind: TokenKind) -> bool {
        self.current() == kind
    }

    #[inline]
    pub(crate) fn at_set(&self, set: &[TokenKind]) -> bool {
        set.contains(&self.current())
    }

    #[inline]
    pub(crate) fn eof(&self) -> bool {
        self.at(TokenKind::Eof)
    }

    pub(crate) fn error(&mut self, message: impl Into<EcoString>) {
        self.errors
            .push(TableGenError::new(self.current_range(), message));
        self.is_after_error = true;
    }

    pub(crate) fn error_and_eat(&mut self, message: impl Into<EcoString>) {
        self.error(message);

        self.builder.start_node(SyntaxKind::Error.into());
        self.eat();
        self.builder.finish_node();
    }

    pub(crate) fn error_and_recover(&mut self, message: impl Into<EcoString>) {
        self.error(message);

        if !self.at_set(self.recover_tokens) && !self.eof() {
            self.builder.start_node(SyntaxKind::Error.into());
            self.eat();
            self.builder.finish_node();
        }
    }

    #[inline]
    pub(crate) fn assert(&mut self, kind: TokenKind) {
        assert!(self.eat_if(kind));
    }

    #[inline]
    pub(crate) fn expect(&mut self, kind: TokenKind) {
        self.expect_with_msg(kind, eco_format!("expected {kind:?}"))
    }

    pub(crate) fn expect_with_msg(&mut self, kind: TokenKind, message: impl Into<EcoString>) {
        if !self.eat_if(kind) && !self.is_after_error {
            self.error(message);
        }
    }

    pub(crate) fn eat(&mut self) {
        self.consume_token();
        self.eat_trivia();
    }

    pub(crate) fn eat_if(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.eat();
            true
        } else {
            false
        }
    }

    pub(crate) fn eat_trivia(&mut self) {
        while self.current().is_trivia() {
            self.consume_token();
        }
    }

    fn consume_token(&mut self) {
        if self.at(TokenKind::Error) {
            let message = self.lexer.take_error().unwrap();
            self.error(message);
        } else {
            self.builder
                .token(self.current().into(), self.current_text());
        }

        self.lexer.next();
        self.is_after_error = false;
    }
}
