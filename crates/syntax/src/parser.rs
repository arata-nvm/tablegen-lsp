use ecow::eco_format;
use rowan::{GreenNode, GreenNodeBuilder};
pub use rowan::{TextRange, TextSize};

use crate::grammar::RECOVER_TOKENS;
use crate::lexer::Lexer;
use crate::preprocessor::PreProcessor;
use crate::token_stream::TokenStream;
use crate::{error::SyntaxError, syntax_kind::SyntaxKind, token_kind::TokenKind};

#[derive(Debug)]
pub(crate) enum CompletedMarker {
    Success,
    Fail,
}

impl CompletedMarker {
    pub(crate) fn is_success(&self) -> bool {
        matches!(self, Self::Success)
    }

    pub(crate) fn or_error(self, parser: &mut Parser, message: impl Into<String>) {
        if !self.is_success() {
            parser.error(message);
        }
    }
}

pub(crate) type Parser<'a> = ParserBase<PreProcessor<Lexer<'a>>>;

#[derive(Debug)]
pub(crate) struct ParserBase<T: TokenStream> {
    token_stream: T,
    builder: GreenNodeBuilder<'static>,

    errors: Vec<SyntaxError>,
    is_after_error: bool,
}

impl<T: TokenStream> ParserBase<T> {
    pub(crate) fn new(token_stream: T) -> Self {
        Self {
            token_stream,
            builder: GreenNodeBuilder::new(),

            errors: Vec::new(),
            is_after_error: false,
        }
    }

    #[inline]
    pub(crate) fn finish(self) -> (GreenNode, Vec<SyntaxError>) {
        (self.builder.finish(), self.errors)
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
    pub(crate) fn peek(&self) -> TokenKind {
        self.token_stream.peek()
    }

    #[inline]
    pub(crate) fn at(&self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    #[inline]
    pub(crate) fn at_set(&self, set: &[TokenKind]) -> bool {
        set.contains(&self.peek())
    }

    #[inline]
    pub(crate) fn eof(&self) -> bool {
        self.at(TokenKind::Eof)
    }

    pub(crate) fn error(&mut self, message: impl Into<String>) {
        self.errors
            .push(SyntaxError::new(self.token_stream.peek_range(), message));
        self.is_after_error = true;
    }

    pub(crate) fn error_and_eat(&mut self, message: impl Into<String>) {
        self.error(message);

        self.builder.start_node(SyntaxKind::Error.into());
        self.eat();
        self.builder.finish_node();
    }

    pub(crate) fn error_and_recover(&mut self, message: impl Into<String>) {
        self.error(message);

        if !self.at_set(&RECOVER_TOKENS) && !self.eof() {
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

    pub(crate) fn expect_with_msg(&mut self, kind: TokenKind, message: impl Into<String>) {
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
        while self.peek().is_trivia() {
            self.consume_token();
        }
    }

    fn consume_token(&mut self) {
        self.builder
            .token(self.peek().into(), self.token_stream.peek_text());
        if self.at(TokenKind::Error) {
            let message = self
                .token_stream
                .take_error()
                .expect("error token without message");
            self.error(message);
        } else {
            self.is_after_error = false;
        }

        self.token_stream.eat();
    }
}
