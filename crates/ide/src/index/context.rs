use ecow::{eco_format, EcoString};
use syntax::parser::TextRange;

use crate::{
    file_system::{FileId, FileRange},
    handlers::diagnostics::Diagnostic,
    symbol_map::{symbol::SymbolId, variable::VariableId, SymbolMap},
};

use super::{scope::Scopes, Index, IndexDatabase};

pub struct IndexCtx<'a> {
    pub db: &'a dyn IndexDatabase,
    pub file_trace: Vec<FileId>,
    pub symbol_map: SymbolMap,
    pub diagnostics: Vec<Diagnostic>,
    pub scopes: Scopes,
    pub anonymous_def_index: u32,
}

impl<'a> IndexCtx<'a> {
    pub fn new(db: &'a dyn IndexDatabase, root_file: FileId) -> Self {
        Self {
            db,
            file_trace: vec![root_file],
            symbol_map: SymbolMap::default(),
            diagnostics: Vec::new(),
            scopes: Scopes::default(),
            anonymous_def_index: 0,
        }
    }

    pub fn current_file_id(&self) -> FileId {
        *self.file_trace.last().expect("file_trace is empty")
    }

    pub fn push_file(&mut self, file_id: FileId) {
        self.file_trace.push(file_id);
    }

    pub fn pop_file(&mut self) {
        self.file_trace.pop().expect("file_trace is empty");
    }

    pub fn resolve_id(&self, name: &EcoString) -> Option<SymbolId> {
        if let Some(symbol_id) = self.scopes.find_local(&self.symbol_map, name) {
            return Some(symbol_id);
        }
        if let Some(def_id) = self.symbol_map.find_def(name) {
            return Some(def_id.into());
        }
        None
    }

    pub fn resolve_id_in_current_scope(&self, name: &EcoString) -> Option<VariableId> {
        self.scopes.find_variable_in_current_scope(name)
    }

    pub fn error(&mut self, range: TextRange, message: impl Into<String>) {
        let file = self.current_file_id();
        self.diagnostics
            .push(Diagnostic::new(FileRange::new(file, range), message));
    }

    pub fn finish(self) -> Index {
        Index {
            symbol_map: self.symbol_map,
            diagnostics: self.diagnostics,
        }
    }

    pub fn next_anonymous_def_name(&mut self) -> EcoString {
        let index = self.anonymous_def_index;
        self.anonymous_def_index += 1;
        eco_format!("anonymous_{index}")
    }
}
