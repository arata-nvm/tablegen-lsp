use std::collections::HashMap;

use ecow::EcoString;
use id_arena::Id;
use tablegen_parser::error::Range;

use crate::document::DocumentId;

pub type Location = (DocumentId, Range);

pub type OldSymbolId = Id<OldSymbol>;

#[derive(Debug)]
pub struct OldSymbol {
    name: EcoString,
    kind: SymbolKind,
    define_loc: Location,
    reference_locs: Vec<Location>,
    template_args: HashMap<EcoString, OldSymbolId>,
    fields: HashMap<EcoString, OldSymbolId>,
    typ: SymbolType,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolKind {
    Record,
    TemplateArg,
    Field,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolType {
    Primitive,
    Record(OldSymbolId),
}

impl OldSymbol {
    pub fn new(name: EcoString, kind: SymbolKind, define_loc: Location, typ: SymbolType) -> Self {
        Self {
            name,
            kind,
            define_loc,
            reference_locs: Vec::new(),
            template_args: HashMap::new(),
            fields: HashMap::new(),
            typ,
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

    pub fn add_template_arg(&mut self, name: EcoString, template_arg_id: OldSymbolId) {
        self.template_args.insert(name, template_arg_id);
    }

    pub fn template_args(&self) -> Vec<&OldSymbolId> {
        self.template_args.values().collect()
    }

    pub fn add_field(&mut self, name: EcoString, field_id: OldSymbolId) {
        self.fields.insert(name, field_id);
    }

    pub fn fields(&self) -> Vec<&OldSymbolId> {
        self.fields.values().collect()
    }

    pub fn find_field(&self, name: &EcoString) -> Option<&OldSymbolId> {
        self.fields.get(name)
    }

    pub fn r#type(&self) -> SymbolType {
        self.typ
    }
}
