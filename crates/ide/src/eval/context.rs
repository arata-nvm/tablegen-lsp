use ecow::EcoString;
use syntax::parser::TextRange;

use crate::{
    file_system::{FileId, FileRange},
    handlers::diagnostics::Diagnostic,
    symbol_map::{symbol::SymbolId, variable::VariableId, SymbolMap},
};

use super::{scope::Scopes, EvalDatabase, Evaluation};

pub struct EvalCtx<'a> {
    pub db: &'a dyn EvalDatabase,
    pub file_trace: Vec<FileId>,
    pub symbol_map: SymbolMap,
    pub diagnostics: Vec<Diagnostic>,
    pub scopes: Scopes,
}

impl<'a> EvalCtx<'a> {
    pub fn new(db: &'a dyn EvalDatabase, root_file: FileId) -> Self {
        Self {
            db,
            file_trace: vec![root_file],
            symbol_map: SymbolMap::default(),
            diagnostics: Vec::new(),
            scopes: Scopes::default(),
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
        if let Some(def_id) = self.symbol_map.find_def(name) {
            return Some(def_id.into());
        }
        if let Some(symbol_id) = self.scopes.find_local(name) {
            return Some(symbol_id);
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

    pub fn finish(self) -> Evaluation {
        Evaluation {
            symbol_map: self.symbol_map,
            diagnostics: self.diagnostics,
        }
    }

    pub fn scopes(&self) -> &Scopes {
        &self.scopes
    }
}
