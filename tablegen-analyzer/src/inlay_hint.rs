use tablegen_parser::parser::{TextRange, TextSize};

use crate::document::Document;

pub struct InlayHint {
    pub position: TextSize,
    pub label: String,
}

impl InlayHint {
    fn new(position: TextSize, label: impl Into<String>) -> Self {
        Self {
            position,
            label: label.into(),
        }
    }
}

pub fn inlay_hint(_doc: &Document, _range: TextRange) -> Option<Vec<InlayHint>> {
    Some(vec![])
}
