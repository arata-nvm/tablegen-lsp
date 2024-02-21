use std::sync::Arc;

use salsa::ParallelDatabase;

use crate::db::{RootDatabase, SourceDatabase};
use crate::file::FileId;
use crate::handlers::diagnostics::Diagnostic;
use crate::handlers::document_symbol::DocumentSymbol;
use crate::handlers::{diagnostics, document_symbol};

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
}

pub struct Analysis {
    db: salsa::Snapshot<RootDatabase>,
}

impl Analysis {
    pub fn snapshot(&self) -> &salsa::Snapshot<RootDatabase> {
        &self.db
    }

    pub fn diagnostics(&self, file_id: FileId) -> Vec<Diagnostic> {
        diagnostics::diagnostics(&*self.db, file_id)
    }

    pub fn document_symbol(&self, file_id: FileId) -> Option<Vec<DocumentSymbol>> {
        document_symbol::document_symbol(&*self.db, file_id)
    }
}
