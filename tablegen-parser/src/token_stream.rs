use ecow::EcoString;

use crate::parser::TextRange;
use crate::token_kind::TokenKind;

pub trait TokenStream {
    fn eat(&mut self) -> TokenKind;

    fn peek(&self) -> TokenKind;

    fn take_error(&mut self) -> Option<EcoString>;

    fn current_range(&self) -> TextRange;

    fn current_text(&self) -> &str;
}
