pub mod index;
pub mod symbol;

use ropey::Rope;
use tablegen_parser::{
    error::{Position, SyntaxError},
    grammar,
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
    errors: Vec<SyntaxError>,
    index: TableGenDocumentIndex,
}

impl TableGenDocument {
    pub fn parse(doc_id: TableGenDocumentId, text: String) -> Self {
        let file = grammar::parse(&text);
        let errors = file.errors().into_iter().cloned().collect();
        let index = TableGenDocumentIndex::create_index(doc_id, &file);

        Self {
            doc_id,
            text: Rope::from_str(&text),
            errors,
            index,
        }
    }

    pub fn take_errors(&mut self) -> Vec<SyntaxError> {
        std::mem::take(&mut self.errors)
    }

    pub fn pos_to_line(&self, pos: Position) -> ropey::Result<usize> {
        self.text.try_char_to_line(pos)
    }

    pub fn line_to_pos(&self, line: usize) -> ropey::Result<Position> {
        self.text.try_line_to_char(line)
    }

    pub fn get_definition(&self, pos: Position) -> Option<Location> {
        let Some(symbol) = self.index.get_symbol_at(pos) else { return None; };
        Some(symbol.define_loc())
    }
}
