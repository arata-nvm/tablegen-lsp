use ecow::EcoString;

use crate::parser::TextRange;
use crate::token_kind::TokenKind;

pub trait TokenStream {
    fn next(&mut self) -> TokenKind;

    fn take_error(&mut self) -> Option<EcoString>;

    fn current(&self) -> TokenKind;

    fn current_range(&self) -> TextRange;

    fn current_text(&self) -> &str;
}
