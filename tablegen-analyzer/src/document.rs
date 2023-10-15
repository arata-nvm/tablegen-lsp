use ropey::Rope;
use tablegen_parser::{error::SyntaxError, grammar, node::SyntaxNode, parser::Position};

use crate::{analyze, hover, symbol::Location, symbol_map::SymbolMap};

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
    errors: Vec<SyntaxError>,
    symbol_map: SymbolMap,
}

impl Document {
    pub fn parse(doc_id: DocumentId, text: String) -> Self {
        let root = grammar::parse(&text);
        let (symbol_map, index_errors) = analyze::analyze(doc_id, &root);

        let mut errors: Vec<SyntaxError> = root.errors().into_iter().cloned().collect();
        errors.extend(index_errors);

        Self {
            doc_id,
            text: Rope::from_str(&text),
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

    pub fn take_errors(&mut self) -> Vec<SyntaxError> {
        std::mem::take(&mut self.errors)
    }

    pub fn symbol_map(&self) -> &SymbolMap {
        &self.symbol_map
    }

    pub fn pos_to_line(&self, pos: Position) -> ropey::Result<usize> {
        self.text.try_char_to_line(pos)
    }

    pub fn line_to_pos(&self, line: usize) -> ropey::Result<Position> {
        self.text.try_line_to_char(line)
    }

    pub fn get_definition(&self, pos: Position) -> Option<Location> {
        let symbol = self.symbol_map.get_symbol_at(pos)?;
        Some(symbol.define_loc().clone())
    }

    pub fn get_references(&self, pos: Position) -> Option<Vec<Location>> {
        let symbol = self.symbol_map.get_symbol_at(pos)?;
        Some(symbol.reference_locs().to_vec())
    }

    pub fn get_hover(&self, pos: Position) -> Option<String> {
        let symbol = self.symbol_map.get_symbol_at(pos)?;
        hover::hover(symbol, &self.root)
    }
}
