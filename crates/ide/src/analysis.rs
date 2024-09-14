use std::collections::HashMap;
use std::sync::Arc;

use salsa::ParallelDatabase;

use crate::db::{RootDatabase, SourceDatabase};
use crate::eval::{self, EvalDatabase};
use crate::file_system::{self, FileId, FileSystem};
use crate::handlers::diagnostics::Diagnostic;
use crate::handlers::document_symbol::DocumentSymbol;
use crate::handlers::{diagnostics, document_symbol};
use crate::line_index::LineIndex;
use crate::symbol_map::SymbolMap;

#[derive(Default)]
pub struct AnalysisHost {
    db: RootDatabase,
}

impl AnalysisHost {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn analysis(&self) -> Analysis {
        Analysis {
            db: self.db.snapshot(),
        }
    }

    pub fn set_file_content(&mut self, file_id: FileId, text: Arc<str>) {
        self.db.set_file_content(file_id, text);
    }

    pub fn set_root_file<FS: FileSystem>(&mut self, fs: &mut FS, root_file: FileId) {
        let source_root = file_system::collect_sources(&mut self.db, fs, root_file);
        self.db.set_source_root(Arc::new(source_root));
    }

    pub fn eval<FS: FileSystem>(&mut self, fs: &mut FS, root_file: FileId) {
        self.db
            .set_symbol_map(root_file, Arc::new(SymbolMap::default()));
        self.db.set_diagnostics(root_file, Arc::new(vec![]));

        match eval::eval(&self.db, fs) {
            Ok(symbol_map) => self.db.set_symbol_map(root_file, Arc::new(symbol_map)),
            Err(diagnostics) => self.db.set_diagnostics(root_file, Arc::new(diagnostics)),
        }
    }
}

pub struct Analysis {
    db: salsa::Snapshot<RootDatabase>,
}

impl Analysis {
    pub fn line_index(&self, file_id: FileId) -> Arc<LineIndex> {
        self.db.line_index(file_id)
    }

    pub fn diagnostics(&self) -> HashMap<FileId, Vec<Diagnostic>> {
        diagnostics::exec(&*self.db)
    }

    pub fn document_symbol(&self, file_id: FileId) -> Option<Vec<DocumentSymbol>> {
        document_symbol::exec(&*self.db, file_id)
    }
}
