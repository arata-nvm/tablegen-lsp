use std::collections::HashMap;

use ecow::{eco_format, EcoString};

use tablegen_parser::language::SyntaxNode;
use tablegen_parser::{error::TableGenError, grammar, parser::TextRange};

use crate::source::Dependencies;
use crate::{
    document::DocumentId,
    symbol::{
        Location, Record, RecordFieldKind, RecordKind, Symbol, SymbolId, SymbolType, VariableKind,
    },
    symbol_map::SymbolMap,
};

#[derive(Debug)]
pub struct DocumentIndexer {
    doc_ids: Vec<DocumentId>,
    dependencies: Dependencies,
    symbol_map: SymbolMap,
    scopes: Vec<HashMap<EcoString, SymbolId>>,
    scope_symbols: Vec<SymbolId>,
    errors: Vec<TableGenError>,
}

impl DocumentIndexer {
    pub fn new(doc_id: DocumentId, dependencies: Dependencies) -> Self {
        Self {
            doc_ids: vec![doc_id],
            dependencies,
            symbol_map: SymbolMap::new(),
            scopes: vec![HashMap::new()],
            scope_symbols: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn finish(self) -> (SymbolMap, Vec<TableGenError>) {
        (self.symbol_map, self.errors)
    }

    pub fn push(&mut self, symbol_id: SymbolId) {
        self.scopes.push(HashMap::new());
        self.scope_symbols.push(symbol_id);
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
        self.scope_symbols.pop();
    }

    pub fn push_temporary(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_temporary(&mut self) {
        self.scopes.pop();
    }

    pub fn error(&mut self, range: TextRange, message: impl Into<EcoString>) {
        self.errors.push(TableGenError::new(range, message));
    }

    fn doc_id(&self) -> DocumentId {
        *self.doc_ids.last().unwrap()
    }

    fn to_location(&self, range: TextRange) -> Location {
        (self.doc_id(), range)
    }

    pub fn add_symbol_reference(&mut self, name: EcoString, range: TextRange) -> Option<SymbolId> {
        // ref: https://llvm.org/docs/TableGen/ProgRef.html#name
        if name == "NAME" {
            return None;
        }

        let reference_loc = self.to_location(range);
        if let Some(symbol_id) = self.find_symbol_scope(&name).copied() {
            self.symbol_map.add_reference(symbol_id, reference_loc);
            Some(symbol_id)
        } else {
            self.error(range, eco_format!("symbol not found: {}", name));
            None
        }
    }

    pub fn symbol(&self, symbol_id: SymbolId) -> Option<&Symbol> {
        self.symbol_map.symbol(symbol_id)
    }

    pub fn add_record(&mut self, name: EcoString, range: TextRange, kind: RecordKind) -> SymbolId {
        let define_loc = self.to_location(range);
        let symbol_id = self.symbol_map.new_record(name.clone(), define_loc, kind);
        self.add_symbol_scope(name.clone(), symbol_id);
        symbol_id
    }

    pub fn add_anonymous_record(&mut self) -> SymbolId {
        let define_loc = self.to_location(TextRange::default());
        self.symbol_map.new_anonymous_record(define_loc)
    }

    pub fn add_template_arg(&mut self, name: EcoString, range: TextRange, typ: SymbolType) {
        let define_loc = self.to_location(range);
        let symbol_id = self.symbol_map.new_record_field(
            name.clone(),
            define_loc,
            RecordFieldKind::TemplateArg,
            typ,
        );
        self.add_symbol_scope(name.clone(), symbol_id);

        let parent = self.scope_symbol_mut();
        parent.add_template_arg(name.clone(), symbol_id);
    }

    pub fn add_parent(&mut self, parent: SymbolId) {
        let cur_record = self.scope_symbol_mut();
        cur_record.add_parent(parent);
    }

    pub fn add_field(&mut self, name: EcoString, range: TextRange, typ: SymbolType) {
        let define_loc = self.to_location(range);
        let symbol_id =
            self.symbol_map
                .new_record_field(name.clone(), define_loc, RecordFieldKind::Field, typ);
        self.add_symbol_scope(name.clone(), symbol_id);

        let parent = self.scope_symbol_mut();
        parent.add_field(name.clone(), symbol_id);
    }

    pub fn add_field_let(&mut self, name: EcoString, range: TextRange, typ: SymbolType) {
        let define_loc = self.to_location(range);
        let symbol_id = self.symbol_map.new_record_field(
            name.clone(),
            define_loc,
            RecordFieldKind::FieldLet,
            typ,
        );
        self.add_symbol_scope(name.clone(), symbol_id);

        let parent = self.scope_symbol_mut();
        parent.add_field(name.clone(), symbol_id);
    }

    pub fn add_variable(
        &mut self,
        name: EcoString,
        range: TextRange,
        kind: VariableKind,
        typ: SymbolType,
    ) {
        let define_loc = self.to_location(range);
        let symbol_id = self
            .symbol_map
            .new_variable(name.clone(), define_loc, kind, typ);
        self.add_symbol_scope(name, symbol_id);
    }

    pub fn add_temporary_variable(&mut self, name: EcoString, range: TextRange, typ: SymbolType) {
        let define_loc = self.to_location(range);
        let symbol_id =
            self.symbol_map
                .new_variable(name.clone(), define_loc, VariableKind::Temporary, typ);
        self.add_symbol_scope(name, symbol_id);
    }

    fn add_symbol_scope(&mut self, name: EcoString, symbol_id: SymbolId) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.clone(), symbol_id);
    }

    pub fn find_symbol_scope(&self, name: &EcoString) -> Option<&SymbolId> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol_id) = scope.get(name) {
                return Some(symbol_id);
            }
        }
        None
    }

    pub fn scope_symbol_id(&self) -> SymbolId {
        self.scope_symbols.last().unwrap().clone()
    }

    pub fn scope_symbol_mut(&mut self) -> &mut Record {
        let symbol_id = self.scope_symbols.last().unwrap();
        self.symbol_map
            .symbol_mut(*symbol_id)
            .and_then(|symbol| symbol.as_record_mut())
            .unwrap()
    }

    pub fn access_field(
        &mut self,
        symbol_id: SymbolId,
        name: EcoString,
        range: TextRange,
    ) -> Option<SymbolId> {
        let Some(field_id) = self.symbol_map.find_field(symbol_id, name.clone()) else {
            self.error(range, eco_format!("cannot access field: {}", name));
            return None;
        };

        let reference_loc = self.to_location(range.clone());
        self.symbol_map.add_reference(field_id, reference_loc);
        Some(field_id)
    }

    pub fn enter_dependency(&mut self, path: String, range: TextRange) -> Option<SyntaxNode> {
        let Some(dependency) = self.dependencies.get(&path) else {
            self.error(range, eco_format!("could not find include file '{path}'"));
            return None;
        };

        self.doc_ids.push(dependency.document_id);

        // TODO: reuse SyntaxNode
        let (root, _) = grammar::parse(&dependency.text);
        Some(root)
    }

    pub fn leave_dependency(&mut self) {
        self.doc_ids.pop();
    }
}
