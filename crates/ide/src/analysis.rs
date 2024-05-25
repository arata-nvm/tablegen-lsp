use std::collections::HashMap;
use std::sync::Arc;

use salsa::ParallelDatabase;

use crate::db::{RootDatabase, SourceDatabase};
use crate::eval::{EvalDatabase, Evaluation};
use crate::file_system::{self, FileId, FilePosition, FileRange, FileSystem};
use crate::handlers::completion::{self, CompletionItem};
use crate::handlers::diagnostics::Diagnostic;
use crate::handlers::document_symbol::DocumentSymbol;
use crate::handlers::hover::Hover;
use crate::handlers::inlay_hint::InlayHint;
use crate::handlers::{
    diagnostics, document_symbol, goto_definition, hover, inlay_hint, references,
};
use crate::line_index::LineIndex;

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
    pub fn line_index(&self, file_id: FileId) -> Arc<LineIndex> {
        self.db.line_index(file_id)
    }

    pub fn eval(&self) -> Arc<Evaluation> {
        self.db.eval()
    }

    pub fn diagnostics(&self) -> HashMap<FileId, Vec<Diagnostic>> {
        diagnostics::exec(&*self.db)
    }

    pub fn document_symbol(&self, file_id: FileId) -> Option<Vec<DocumentSymbol>> {
        document_symbol::exec(&*self.db, file_id)
    }

    pub fn goto_definition(&self, pos: FilePosition) -> Option<FileRange> {
        goto_definition::exec(&*self.db, pos)
    }

    pub fn references(&self, pos: FilePosition) -> Option<Vec<FileRange>> {
        references::exec(&*self.db, pos)
    }

    pub fn hover(&self, pos: FilePosition) -> Option<Hover> {
        hover::exec(&*self.db, pos)
    }

    pub fn inlay_hint(&self, range: FileRange) -> Option<Vec<InlayHint>> {
        inlay_hint::exec(&*self.db, range)
    }

    pub fn completion(&self, pos: FilePosition) -> Option<Vec<CompletionItem>> {
        completion::exec(&*self.db, pos)
    }
}
