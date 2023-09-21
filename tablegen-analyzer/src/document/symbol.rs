use ecow::EcoString;
use id_arena::Id;
use tablegen_parser::error::Range;

use super::TableGenDocumentId;

pub type Location = (TableGenDocumentId, Range);

pub type TableGenSymbolId = Id<TableGenSymbol>;

#[derive(Debug)]
pub struct TableGenSymbol {
    name: EcoString,
    define_loc: Location,
}

impl TableGenSymbol {
    pub fn new(name: EcoString, doc_id: TableGenDocumentId, define_loc: Range) -> Self {
        Self {
            name,
            define_loc: (doc_id, define_loc),
        }
    }

    pub fn define_loc(&self) -> Location {
        self.define_loc.clone()
    }
}
