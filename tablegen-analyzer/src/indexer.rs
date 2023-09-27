use std::collections::HashMap;

use ecow::{eco_format, EcoString};
use tablegen_parser::error::{Range, SyntaxError};

use crate::{
    document::DocumentId,
    symbol::{Location, OldSymbol, OldSymbolId, SymbolKind, SymbolType},
    symbol_map::SymbolMap,
};

#[derive(Debug)]
pub struct DocumentIndexer {
    doc_id: DocumentId,
    symbols: SymbolMap,
    scopes: Vec<HashMap<EcoString, OldSymbolId>>,
    scope_symbols: Vec<OldSymbolId>,
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

    pub fn push(&mut self, symbol_id: OldSymbolId) {
        self.scopes.push(HashMap::new());
        self.scope_symbols.push(symbol_id);
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
        self.scope_symbols.pop();
    }

    pub fn error(&mut self, range: Range, message: impl Into<EcoString>) {
        self.errors.push(SyntaxError::new(range, message));
    }

    fn to_location(&self, range: Range) -> Location {
        (self.doc_id, range)
    }

    fn add_symbol(
        &mut self,
        name: &EcoString,
        range: Range,
        kind: SymbolKind,
        typ: SymbolType,
    ) -> OldSymbolId {
        let define_loc = self.to_location(range);
        let symbol_id = self.symbols.new_symbol(name.clone(), kind, define_loc, typ);
        self.add_symbol_scope(name.clone(), symbol_id);
        symbol_id
    }

    pub fn add_symbol_reference(&mut self, name: &EcoString, range: Range) -> Option<OldSymbolId> {
        let reference_loc = (self.doc_id, range.clone());
        if let Some(symbol_id) = self.find_symbol_scope(name).copied() {
            self.symbols.add_reference(symbol_id, reference_loc);
            Some(symbol_id)
        } else {
            self.error(range, eco_format!("variable not found: {}", name));
            None
        }
    }

    pub fn add_record(&mut self, name: &EcoString, range: Range) -> OldSymbolId {
        self.add_symbol(name, range, SymbolKind::Record, SymbolType::Primitive)
    }

    pub fn add_template_arg(&mut self, name: &EcoString, range: Range, typ: SymbolType) {
        let template_arg_id = self.add_symbol(name, range, SymbolKind::TemplateArg, typ);
        let parent = self.scope_symbol_mut();
        parent.add_template_arg(name.clone(), template_arg_id);
    }

    pub fn add_field(&mut self, name: &EcoString, range: Range, typ: SymbolType) {
        let field_id = self.add_symbol(name, range, SymbolKind::Field, typ);
        let parent = self.scope_symbol_mut();
        parent.add_field(name.clone(), field_id);
    }

    fn add_symbol_scope(&mut self, name: EcoString, symbol_id: OldSymbolId) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.clone(), symbol_id);
    }

    fn find_symbol_scope(&self, name: &EcoString) -> Option<&OldSymbolId> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol_id) = scope.get(name) {
                return Some(symbol_id);
            }
        }
        None
    }

    pub fn scope_symbol_mut(&mut self) -> &mut OldSymbol {
        let symbol_id = self.scope_symbols.last().unwrap();
        self.symbols.symbol_mut(*symbol_id).unwrap()
    }

    pub fn access_field(
        &mut self,
        symbol_id: OldSymbolId,
        name: EcoString,
        range: Range,
    ) -> Option<OldSymbolId> {
        let reference_loc = self.to_location(range.clone());
        let Some(field_id )= self.find_field_of(symbol_id, &name) else {
            self.error(range, eco_format!("cannot access field: {}", name));
            return None;
        };
        self.symbols.add_reference(field_id, reference_loc);
        Some(field_id)
    }

    pub fn find_field_of(&self, symbol_id: OldSymbolId, name: &EcoString) -> Option<OldSymbolId> {
        let symbol = self.symbols.symbol(symbol_id)?;
        let SymbolType::Record(typ_id) = symbol.r#type() else { return None; };
        let typ = self.symbols.symbol(typ_id)?;
        let field_id = typ.find_field(name)?;
        Some(*field_id)
    }
}
