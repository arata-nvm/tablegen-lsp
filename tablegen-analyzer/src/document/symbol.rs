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
    children: HashMap<EcoString, SymbolId>,

    template_args: HashMap<EcoString, SymbolId>,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolKind {
    Class,
    TemplateArg,
    Field,
    Def,
}

impl Symbol {
    pub fn new(name: EcoString, kind: SymbolKind, define_loc: Location) -> Self {
        Self {
            name,
            kind,
            define_loc,
            reference_locs: Vec::new(),
            children: HashMap::new(),

            template_args: HashMap::new(),
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

    pub fn add_child(&mut self, name: EcoString, child: SymbolId) {
        self.children.insert(name, child);
    }

    pub fn children(&self) -> Vec<SymbolId> {
        self.children.values().cloned().collect()
    }

    pub fn add_template_arg(&mut self, name: EcoString, template_arg_id: SymbolId) {
        self.template_args.insert(name, template_arg_id);
    }
}
