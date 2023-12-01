use ecow::EcoString;

use crate::parser::TextRange;
use crate::token_kind::TokenKind;

pub trait TokenStream {
    fn eat(&mut self) -> TokenKind;

    fn peek(&self) -> TokenKind;

    fn peek_range(&self) -> TextRange;

    fn peek_text(&self) -> &str;

    fn take_error(&mut self) -> Option<EcoString>;
}
