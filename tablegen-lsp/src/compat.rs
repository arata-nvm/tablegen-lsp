pub mod analyzer2lsp {
    use tablegen_analyzer::{
        document::Document,
        symbol::{Location, Symbol, SymbolId},
    };
    use tablegen_parser::{error, parser};

    use crate::document_map::DocumentMap;

    pub fn range(doc: &Document, range: parser::TextRange) -> lsp_types::Range {
        let start = position(doc, range.start());
        let end = position(doc, range.end());
        lsp_types::Range::new(start, end)
    }

    pub fn position(doc: &Document, pos: parser::TextSize) -> lsp_types::Position {
        let line = doc.pos_to_line(pos).unwrap_or_default();
        let line_first = doc.line_to_pos(line).unwrap_or_default();
        let character = pos - line_first;
        lsp_types::Position::new(line as u32, character.into())
    }

    pub fn location(doc_map: &DocumentMap, doc: &Document, loc: Location) -> lsp_types::Location {
        let uri = doc_map.to_uri(loc.0).unwrap().clone();
        let range = range(doc, loc.1);
        lsp_types::Location::new(uri, range)
    }

    pub fn error(doc: &Document, error: error::SyntaxError) -> lsp_types::Diagnostic {
        let range = range(doc, error.range);
        let message = error.message.to_string();
        lsp_types::Diagnostic::new_simple(range, message)
    }

    #[allow(deprecated)]
    pub fn document_symbol(doc: &Document, symbol: &Symbol) -> lsp_types::DocumentSymbol {
        let template_args = document_symbol_field(doc, symbol.as_record().template_args());
        let fields = document_symbol_field(doc, symbol.as_record().fields());
        let children: Vec<lsp_types::DocumentSymbol> = template_args.chain(fields).collect();

        let define_loc = range(doc, symbol.define_loc().1.clone());

        lsp_types::DocumentSymbol {
            name: symbol.name().to_string(),
            detail: None,
            kind: lsp_types::SymbolKind::CLASS, // TODO
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

    #[allow(deprecated)]
    fn document_symbol_field<'a>(
        doc: &'a Document,
        symbols: Vec<&'a SymbolId>,
    ) -> impl Iterator<Item = lsp_types::DocumentSymbol> + 'a {
        symbols
            .into_iter()
            .filter_map(|id| doc.symbol_map().symbol(*id))
            .map(|child| lsp_types::DocumentSymbol {
                name: child.name().to_string(),
                detail: None,
                kind: lsp_types::SymbolKind::PROPERTY,
                tags: None,
                deprecated: None,
                range: range(doc, child.define_loc().1.clone()),
                selection_range: range(doc, child.define_loc().1.clone()),
                children: None,
            })
    }

    pub fn hover(hover: String) -> lsp_types::Hover {
        lsp_types::Hover {
            contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(hover)),
            range: None,
        }
    }
}

pub mod lsp2analyzer {
    use tablegen_analyzer::document::Document;
    use tablegen_parser::parser;

    pub fn position(doc: &Document, position: lsp_types::Position) -> parser::TextSize {
        let pos_size = doc.line_to_pos(position.line as usize).unwrap_or_default();
        let char_size: parser::TextSize = position.character.into();
        pos_size + char_size
    }
}
