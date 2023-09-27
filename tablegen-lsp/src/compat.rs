pub mod analyzer2lsp {
    use tablegen_analyzer::document::{
        symbol::{Location, Symbol, SymbolKind},
        Document,
    };
    use tablegen_parser::error;
    use tower_lsp::lsp_types::{self, Diagnostic};

    use crate::document_map::DocumentMap;

    pub fn range(doc: &Document, range: error::Range) -> lsp_types::Range {
        let start = position(doc, range.start);
        let end = position(doc, range.end);
        lsp_types::Range::new(start, end)
    }

    pub fn position(doc: &Document, pos: error::Position) -> lsp_types::Position {
        let line = doc.pos_to_line(pos).unwrap_or_default();
        let line_first = doc.line_to_pos(line).unwrap_or_default();
        let character = pos - line_first;
        lsp_types::Position::new(line as u32, character as u32)
    }

    pub fn location(doc_map: &DocumentMap, doc: &Document, loc: Location) -> lsp_types::Location {
        let uri = doc_map.to_uri(loc.0).unwrap().clone();
        let range = range(doc, loc.1);
        lsp_types::Location::new(uri, range)
    }

    pub fn error(doc: &Document, error: error::SyntaxError) -> Diagnostic {
        let range = range(doc, error.range);
        let message = error.message.to_string();
        Diagnostic::new_simple(range, message)
    }

    #[allow(deprecated)]
    pub fn document_symbol(doc: &Document, symbol: &Symbol) -> lsp_types::DocumentSymbol {
        let template_args = symbol
            .template_args()
            .into_iter()
            .filter_map(|id| doc.index().symbol(*id))
            .map(|child| document_symbol(doc, child));

        let fields = symbol
            .fields()
            .into_iter()
            .filter_map(|id| doc.index().symbol(*id))
            .map(|child| document_symbol(doc, child));

        let children: Vec<lsp_types::DocumentSymbol> = template_args.chain(fields).collect();

        let define_loc = range(doc, symbol.define_loc().1);

        lsp_types::DocumentSymbol {
            name: symbol.name().to_string(),
            detail: None,
            kind: symbol_kind(symbol.kind()),
            tags: None,
            deprecated: None,
            range: define_loc,
            selection_range: define_loc,
            children: if children.len() > 0 {
                Some(children)
            } else {
                None
            },
        }
    }

    pub fn symbol_kind(kind: SymbolKind) -> lsp_types::SymbolKind {
        match kind {
            SymbolKind::Class => lsp_types::SymbolKind::CLASS,
            SymbolKind::TemplateArg => lsp_types::SymbolKind::PROPERTY,
            SymbolKind::Field => lsp_types::SymbolKind::FIELD,
            SymbolKind::Def => lsp_types::SymbolKind::CLASS,
        }
    }
}

pub mod lsp2analyzer {
    use tablegen_analyzer::document::Document;
    use tablegen_parser::error;
    use tower_lsp::lsp_types;

    pub fn position(doc: &Document, position: lsp_types::Position) -> error::Position {
        doc.line_to_pos(position.line as usize).unwrap_or_default() + position.character as usize
    }
}
