pub mod analyzer2lsp {
    use tablegen_analyzer::inlay_hint::InlayHintKind;
    use tablegen_analyzer::{
        completion::{CompletionItem, CompletionItemKind},
        document::Document,
        inlay_hint::InlayHint,
        symbol::{Location, Symbol, SymbolId},
    };
    use tablegen_parser::{error, parser};

    use crate::server::DocumentMap;

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
        let uri = doc_map.to_path(loc.0).unwrap().clone();
        let range = range(doc, loc.1);
        lsp_types::Location::new(uri, range)
    }

    pub fn error(doc: &Document, error: error::TableGenError) -> lsp_types::Diagnostic {
        let range = range(doc, error.range);
        let message = error.message.to_string();
        lsp_types::Diagnostic::new_simple(range, message)
    }

    #[allow(deprecated)]
    pub fn document_symbol(doc: &Document, symbol: &Symbol) -> lsp_types::DocumentSymbol {
        let (children, kind) = match symbol {
            Symbol::Record(record) => {
                let template_args = document_symbols(doc, record.template_args());
                let fields = document_symbols(doc, record.fields());
                let children: Vec<lsp_types::DocumentSymbol> =
                    template_args.chain(fields).collect();
                (children, lsp_types::SymbolKind::CLASS)
            }
            Symbol::RecordField(_) => (vec![], lsp_types::SymbolKind::PROPERTY),
            Symbol::Variable(_) => (vec![], lsp_types::SymbolKind::CONSTANT),
        };
        let define_loc = range(doc, symbol.define_loc().1);

        lsp_types::DocumentSymbol {
            name: symbol.name().to_string(),
            detail: None,
            kind,
            tags: None,
            deprecated: None,
            range: define_loc,
            selection_range: define_loc,
            children: if children.is_empty() {
                Some(children)
            } else {
                None
            },
        }
    }

    fn document_symbols<'a>(
        doc: &'a Document,
        symbols: Vec<SymbolId>,
    ) -> impl Iterator<Item = lsp_types::DocumentSymbol> + 'a {
        symbols
            .into_iter()
            .filter_map(|id| doc.symbol_map().symbol(id))
            .map(|symbol| document_symbol(doc, symbol))
    }

    pub fn hover(hover: String) -> lsp_types::Hover {
        lsp_types::Hover {
            contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(hover)),
            range: None,
        }
    }

    pub fn completion_item(item: CompletionItem) -> lsp_types::CompletionItem {
        let mut lsp_item = lsp_types::CompletionItem::new_simple(item.label, item.detail);
        lsp_item.kind = Some(match item.kind {
            CompletionItemKind::Keyword => lsp_types::CompletionItemKind::KEYWORD,
            CompletionItemKind::Type => lsp_types::CompletionItemKind::CLASS,
            CompletionItemKind::Class => lsp_types::CompletionItemKind::CLASS,
            CompletionItemKind::Def => lsp_types::CompletionItemKind::VARIABLE,
            CompletionItemKind::Defset => lsp_types::CompletionItemKind::VARIABLE,
            CompletionItemKind::Defvar => lsp_types::CompletionItemKind::VARIABLE,
            CompletionItemKind::Field => lsp_types::CompletionItemKind::FIELD,
            CompletionItemKind::TemplateArg => lsp_types::CompletionItemKind::PROPERTY,
        });
        lsp_item
    }

    pub fn inlay_hint(doc: &Document, hint: InlayHint) -> lsp_types::InlayHint {
        lsp_types::InlayHint {
            position: position(doc, hint.position),
            label: lsp_types::InlayHintLabel::String(hint.label),
            kind: None,
            text_edits: None,
            tooltip: None,
            padding_left: match hint.kind {
                InlayHintKind::TemplateArg => None,
                InlayHintKind::FieldLet => Some(true),
            },
            padding_right: match hint.kind {
                InlayHintKind::TemplateArg => Some(true),
                InlayHintKind::FieldLet => None,
            },
            data: None,
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

    pub fn range(doc: &Document, range: lsp_types::Range) -> parser::TextRange {
        let start = position(doc, range.start);
        let end = position(doc, range.end);
        parser::TextRange::new(start, end)
    }
}
