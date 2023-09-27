use std::collections::HashMap;

use ecow::{eco_format, EcoString};
use tablegen_parser::error::{Range, SyntaxError};

use crate::{
    document::DocumentId,
    symbol::{Location, Symbol, SymbolId, SymbolKind},
    symbol_map::SymbolMap,
};

#[derive(Debug)]
pub struct DocumentIndexer {
    doc_id: DocumentId,
    symbols: SymbolMap,
    scopes: Vec<HashMap<EcoString, SymbolId>>,
    scope_symbols: Vec<SymbolId>,
    errors: Vec<SyntaxError>,
}

impl DocumentIndexer {
    pub fn new(doc_id: DocumentId) -> Self {
        Self {
            doc_id,
            symbols: SymbolMap::new(),
            scopes: vec![HashMap::new()],
            scope_symbols: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn finish(self) -> (SymbolMap, Vec<SyntaxError>) {
        (self.symbols, self.errors)
    }

    pub fn push(&mut self, symbol_id: SymbolId) {
        self.scopes.push(HashMap::new());
        self.scope_symbols.push(symbol_id);
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
        self.scope_symbols.pop();
    }

    fn to_location(&self, range: Range) -> Location {
        (self.doc_id, range)
    }

    fn add_symbol(&mut self, name: &EcoString, range: Range, kind: SymbolKind) -> SymbolId {
        let define_loc = self.to_location(range);
        let symbol_id = self.symbols.new_symbol(name.clone(), kind, define_loc);
        self.add_symbol_scope(name.clone(), symbol_id);
        symbol_id
    }

    pub fn add_symbol_reference(&mut self, name: &EcoString, range: Range) {
        let reference_loc = (self.doc_id, range.clone());
        if let Some(symbol_id) = self.find_symbol_scope(name) {
            self.symbols.add_reference(*symbol_id, reference_loc);
        } else {
            self.errors.push(SyntaxError::new(
                range,
                eco_format!("variable not found: {}", name),
            ));
        };
    }

    pub fn add_record(&mut self, name: &EcoString, range: Range) -> SymbolId {
        self.add_symbol(name, range, SymbolKind::Record)
    }

    pub fn add_template_arg(&mut self, name: &EcoString, range: Range) {
        let template_arg_id = self.add_symbol(name, range, SymbolKind::TemplateArg);
        let parent = self.scope_symbol_mut();
        parent.add_template_arg(name.clone(), template_arg_id);
    }

    pub fn add_field(&mut self, name: &EcoString, range: Range) {
        let field_id = self.add_symbol(name, range, SymbolKind::Field);
        let parent = self.scope_symbol_mut();
        parent.add_field(name.clone(), field_id);
    }

    fn add_symbol_scope(&mut self, name: EcoString, symbol_id: SymbolId) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.clone(), symbol_id);
    }

    fn find_symbol_scope(&self, name: &EcoString) -> Option<&SymbolId> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol_id) = scope.get(name) {
                return Some(symbol_id);
            }
        }
        None
    }

    pub fn scope_symbol_mut(&mut self) -> &mut Symbol {
        let symbol_id = self.scope_symbols.last().unwrap();
        self.symbols.symbol_mut(*symbol_id).unwrap()
    }
}
