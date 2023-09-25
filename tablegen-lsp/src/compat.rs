pub mod analyzer2lsp {
    use tablegen_analyzer::document::{symbol::Location, TableGenDocument};
    use tablegen_parser::error;
    use tower_lsp::lsp_types::{self, Diagnostic};

    use crate::document_map::TableGenDocumentMap;

    pub fn range(doc: &TableGenDocument, range: error::Range) -> lsp_types::Range {
        let start = position(doc, range.start);
        let end = position(doc, range.end);
        lsp_types::Range::new(start, end)
    }

    pub fn position(doc: &TableGenDocument, pos: error::Position) -> lsp_types::Position {
        let line = doc.pos_to_line(pos).unwrap_or_default();
        let line_first = doc.line_to_pos(line).unwrap_or_default();
        let character = pos - line_first;
        lsp_types::Position::new(line as u32, character as u32)
    }

    pub fn location(
        doc_map: &TableGenDocumentMap,
        doc: &TableGenDocument,
        loc: Location,
    ) -> lsp_types::Location {
        let uri = doc_map.to_uri(loc.0).unwrap().clone();
        let range = range(doc, loc.1);
        lsp_types::Location::new(uri, range)
    }

    pub fn error(doc: &TableGenDocument, error: error::SyntaxError) -> Diagnostic {
        let range = range(doc, error.range);
        let message = error.message.to_string();
        Diagnostic::new_simple(range, message)
    }
}

pub mod lsp2analyzer {
    use tablegen_analyzer::document::TableGenDocument;
    use tablegen_parser::error;
    use tower_lsp::lsp_types;

    pub fn position(doc: &TableGenDocument, position: lsp_types::Position) -> error::Position {
        doc.line_to_pos(position.line as usize).unwrap_or_default() + position.character as usize
    }
}
