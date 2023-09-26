use std::collections::HashMap;

use ecow::EcoString;
use id_arena::Id;
use tablegen_parser::error::Range;

use super::TableGenDocumentId;

pub type Location = (TableGenDocumentId, Range);

pub type TableGenSymbolId = Id<TableGenSymbol>;

#[derive(Debug)]
pub struct TableGenSymbol {
    name: EcoString,
    kind: TableGenSymbolKind,
    define_loc: Location,
    reference_locs: Vec<Location>,
    children: HashMap<EcoString, TableGenSymbolId>,
}

#[derive(Debug)]
pub enum TableGenSymbolKind {
    Class,
    TemplateArg,
    Field,
    Def,
}

impl TableGenSymbol {
    pub fn new(name: EcoString, kind: TableGenSymbolKind, define_loc: Location) -> Self {
        Self {
            name,
            kind,
            define_loc,
            reference_locs: Vec::new(),
            children: HashMap::new(),
        }
    }

    pub fn name(&self) -> EcoString {
        self.name.clone()
    }

    pub fn kind(&self) -> TableGenSymbolKind {
        self.kind
    }

    pub fn define_loc(&self) -> Location {
        self.define_loc.clone()
    }

    pub fn add_reference(&mut self, loc: Location) {
        self.reference_locs.push(loc);
    }

    pub fn reference_locs(&self) -> Vec<Location> {
        self.reference_locs.clone()
    }

    pub fn add_child(&mut self, name: EcoString, child: TableGenSymbolId) {
        self.children.insert(name, child);
    }

    pub fn children(&self) -> Vec<TableGenSymbolId> {
        self.children.values().cloned().collect()
    }
}
