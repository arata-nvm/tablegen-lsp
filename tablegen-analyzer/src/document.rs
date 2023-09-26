pub mod index;
pub mod symbol;

use ropey::Rope;
use tablegen_parser::{
    error::{Position, SyntaxError},
    grammar,
    node::SyntaxNode,
};

use self::{index::TableGenDocumentIndex, symbol::Location};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TableGenDocumentId(usize);

impl TableGenDocumentId {
    pub fn new(doc_id: usize) -> Self {
        Self(doc_id)
    }
}

impl From<TableGenDocumentId> for usize {
    fn from(value: TableGenDocumentId) -> Self {
        value.0
    }
}

#[derive(Debug)]
pub struct TableGenDocument {
    doc_id: TableGenDocumentId,
    text: Rope,
    root: SyntaxNode,
    errors: Vec<SyntaxError>,
    index: TableGenDocumentIndex,
}

impl TableGenDocument {
    pub fn parse(doc_id: TableGenDocumentId, text: String) -> Self {
        let root = grammar::parse(&text);
        let mut index = TableGenDocumentIndex::create_index(doc_id, &root);

        let mut errors: Vec<SyntaxError> = root.errors().into_iter().cloned().collect();
        errors.extend(index.take_errors());

        Self {
            doc_id,
            text: Rope::from_str(&text),
            root,
            errors,
            index,
        }
    }

    pub fn id(&self) -> TableGenDocumentId {
        self.doc_id
    }

    pub fn root(&self) -> &SyntaxNode {
        &self.root
    }

    pub fn take_errors(&mut self) -> Vec<SyntaxError> {
        std::mem::take(&mut self.errors)
    }

    pub fn index(&self) -> &TableGenDocumentIndex {
        &self.index
    }

    pub fn pos_to_line(&self, pos: Position) -> ropey::Result<usize> {
        self.text.try_char_to_line(pos)
    }

    pub fn line_to_pos(&self, line: usize) -> ropey::Result<Position> {
        self.text.try_line_to_char(line)
    }

    pub fn get_definition(&self, pos: Position) -> Option<Location> {
        let symbol = self.index.get_symbol_at(pos)?;
        Some(symbol.define_loc())
    }

    pub fn get_references(&self, pos: Position) -> Option<Vec<Location>> {
        let symbol = self.index.get_symbol_at(pos)?;
        Some(symbol.reference_locs())
    }
}
