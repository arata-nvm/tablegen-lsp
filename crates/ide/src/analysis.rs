use std::collections::HashMap;
use std::sync::Arc;

use salsa::ParallelDatabase;

use crate::db::{RootDatabase, SourceDatabase};
use crate::file_system::{self, FileId, FileSystem};
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

    pub fn set_root_file<FS: FileSystem>(&mut self, fs: &mut FS, root_file: FileId) {
        let source_root = file_system::collect_sources(&mut self.db, fs, root_file);
        self.db.set_source_root(Arc::new(source_root));
    }
}

pub struct Analysis {
    db: salsa::Snapshot<RootDatabase>,
}

impl Analysis {
    pub fn snapshot(&self) -> &salsa::Snapshot<RootDatabase> {
        &self.db
    }

    pub fn diagnostics(&self) -> HashMap<FileId, Vec<Diagnostic>> {
        diagnostics::exec(&*self.db)
    }

    pub fn document_symbol(&self, file_id: FileId) -> Option<Vec<DocumentSymbol>> {
        document_symbol::exec(&*self.db, file_id)
    }
}
