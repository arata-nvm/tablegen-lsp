use std::collections::HashMap;

use ecow::EcoString;
use id_arena::Id;
use tablegen_parser::error::Range;

use super::DocumentId;

pub type Location = (DocumentId, Range);

pub type SymbolId = Id<Symbol>;

#[derive(Debug)]
pub struct Symbol {
    name: EcoString,
    kind: SymbolKind,
    define_loc: Location,
    reference_locs: Vec<Location>,
    template_args: HashMap<EcoString, SymbolId>,
    fields: HashMap<EcoString, SymbolId>,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolKind {
    Record,
    TemplateArg,
    Field,
}

impl Symbol {
    pub fn new(name: EcoString, kind: SymbolKind, define_loc: Location) -> Self {
        Self {
            name,
            kind,
            define_loc,
            reference_locs: Vec::new(),
            template_args: HashMap::new(),
            fields: HashMap::new(),
        }
    }

    pub fn name(&self) -> EcoString {
        self.name.clone()
    }

    pub fn kind(&self) -> SymbolKind {
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

    pub fn add_template_arg(&mut self, name: EcoString, template_arg_id: SymbolId) {
        self.template_args.insert(name, template_arg_id);
    }

    pub fn template_args(&self) -> Vec<&SymbolId> {
        self.template_args.values().collect()
    }

    pub fn add_field(&mut self, name: EcoString, field_id: SymbolId) {
        self.fields.insert(name, field_id);
    }

    pub fn fields(&self) -> Vec<&SymbolId> {
        self.fields.values().collect()
    }
}
