use std::ops::Range;

use ecow::EcoString;

use crate::token_kind::TokenKind;

pub trait TokenStream {
    fn eat(&mut self) -> TokenKind;

    fn cursor(&self) -> usize;

    fn text(&self, range: Range<usize>) -> &str;

    fn take_error(&mut self) -> Option<EcoString>;
}
