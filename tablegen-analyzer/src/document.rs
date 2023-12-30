use ropey::Rope;

use tablegen_parser::parser::TextRange;
use tablegen_parser::{error::TableGenError, grammar, language::SyntaxNode, parser::TextSize};

use crate::completion::CompletionItem;
use crate::inlay_hint::InlayHint;
use crate::source::{Dependencies, SourceSet};
use crate::{analyze, completion, hover, inlay_hint, symbol::Location, symbol_map::SymbolMap};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DocumentId(usize);

impl DocumentId {
    pub fn new(doc_id: usize) -> Self {
        Self(doc_id)
    }
}

impl From<DocumentId> for usize {
    fn from(value: DocumentId) -> Self {
        value.0
    }
}

#[derive(Debug)]
pub struct Document {
    doc_id: DocumentId,
    text: Rope,
    root: SyntaxNode,
    errors: Vec<TableGenError>,
    symbol_map: SymbolMap,
}

impl Document {
    pub fn parse(doc_id: DocumentId, text: &str) -> Self {
        let mut errors = Vec::new();

        let (root, parse_errors) = grammar::parse(text);
        errors.extend(parse_errors);

        let (symbol_map, index_errors) =
            analyze::analyze(doc_id, Dependencies::new(), root.clone());
        errors.extend(index_errors);

        Self {
            doc_id,
            text: Rope::from_str(text),
            root,
            errors,
            symbol_map,
        }
    }

    pub fn parse_set(source_set: SourceSet) -> Self {
        let mut errors = Vec::new();

        let source_root = source_set.root;

        let (root, parse_errors) = grammar::parse(&source_root.text);
        errors.extend(parse_errors);

        let (symbol_map, index_errors) = analyze::analyze(
            source_root.document_id,
            source_set.dependencies,
            root.clone(),
        );
        errors.extend(index_errors);

        Self {
            doc_id: source_root.document_id,
            text: Rope::from_str(&source_root.text),
            root,
            errors,
            symbol_map,
        }
    }

    pub fn id(&self) -> DocumentId {
        self.doc_id
    }

    pub fn root(&self) -> &SyntaxNode {
        &self.root
    }

    pub fn take_errors(&mut self) -> Vec<TableGenError> {
        std::mem::take(&mut self.errors)
    }

    pub fn symbol_map(&self) -> &SymbolMap {
        &self.symbol_map
    }

    pub fn pos_to_line(&self, pos: TextSize) -> ropey::Result<usize> {
        self.text.try_char_to_line(pos.into())
    }

    pub fn line_to_pos(&self, line: usize) -> ropey::Result<TextSize> {
        self.text
            .try_line_to_char(line)
            .map(|pos| pos.try_into().unwrap())
    }

    pub fn get_definition(&self, pos: TextSize) -> Option<Location> {
        let symbol = self.symbol_map.get_symbol_at(pos)?;
        Some(symbol.define_loc().clone())
    }

    pub fn get_references(&self, pos: TextSize) -> Option<Vec<Location>> {
        let symbol = self.symbol_map.get_symbol_at(pos)?;
        Some(symbol.reference_locs().to_vec())
    }

    pub fn get_hover(&self, pos: TextSize) -> Option<String> {
        let symbol = self.symbol_map.get_symbol_at(pos)?;
        hover::hover(symbol, self.root.clone())
    }

    pub fn get_completion(&self, pos: TextSize) -> Option<Vec<CompletionItem>> {
        completion::completion(self, pos)
    }

    pub fn get_inlay_hint(&self, range: TextRange) -> Option<Vec<InlayHint>> {
        inlay_hint::inlay_hint(self, range)
    }
}
