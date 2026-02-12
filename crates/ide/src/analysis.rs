use std::panic::AssertUnwindSafe;
use std::sync::Arc;

use salsa::Cancelled;

use crate::db::{Database, IndexDatabase, RootDatabase, SourceDatabase};
use crate::file_system::{
    self, FileId, FilePath, FilePosition, FileRange, FileSystem, SourceUnit, SourceUnitId,
};
use crate::handlers::completion::{self, CompletionItem};
use crate::handlers::diagnostics::Diagnostic;
use crate::handlers::document_link::DocumentLink;
use crate::handlers::document_symbol::DocumentSymbol;
use crate::handlers::folding_range::FoldingRange;
use crate::handlers::hover::Hover;
use crate::handlers::inlay_hint::InlayHint;
use crate::handlers::{
    diagnostics, document_link, document_symbol, folding_range, goto_definition, hover, inlay_hint,
    references,
};
use crate::index::Index;
use crate::interop::TblgenParseResult;
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
            db: self.db.clone(),
        }
    }

    pub fn set_file_content(&mut self, file_id: FileId, text: Arc<str>) {
        self.db.set_file(file_id, text);
    }

    pub fn set_tblgen_parse_result(
        &mut self,
        source_unit_id: SourceUnitId,
        result: TblgenParseResult,
    ) {
        self.db.set_tblgen_result(source_unit_id, result);
    }

    pub fn load_source_unit<FS: FileSystem>(
        &mut self,
        fs: &mut FS,
        root_file: FileId,
        include_dirs: &[FilePath],
    ) -> SourceUnitId {
        let id = SourceUnitId::from_root_file(root_file);
        let source_unit = file_system::collect_sources(&mut self.db, fs, root_file, include_dirs);
        self.db.set_source_unit(id, Arc::from(source_unit));
        id
    }
}

pub struct Analysis {
    db: RootDatabase,
}

pub type Cancellable<T> = Result<T, Cancelled>;

impl Analysis {
    fn with_db<F, T>(&self, f: F) -> Cancellable<T>
    where
        F: FnOnce(&RootDatabase) -> T + std::panic::UnwindSafe,
    {
        // NOTE:
        // AssertUnwindSafe is necessary because `RootDatabase` uses `DashMap` which is not unwind safe.
        // ref: https://salsa.zulipchat.com/#narrow/stream/145099-general/topic/How.20to.20use.20.60Cancelled.3A.3Acatch.60
        let db = AssertUnwindSafe(&self.db);
        Cancelled::catch(|| f(*db))
    }

    pub fn line_index(&self, file_id: FileId) -> Cancellable<Arc<LineIndex>> {
        self.with_db(|db| db.line_index(file_id))
    }

    pub fn index(&self, source_unit_id: SourceUnitId) -> Cancellable<Arc<Index>> {
        self.with_db(|db| db.index(source_unit_id))
    }

    pub fn source_unit(&self, source_unit_id: SourceUnitId) -> Cancellable<Arc<SourceUnit>> {
        self.with_db(|db| Arc::clone(db.source_unit(source_unit_id).source_unit(db)))
    }

    pub fn diagnostics(&self, source_unit_id: SourceUnitId) -> Cancellable<Vec<Diagnostic>> {
        self.with_db(|db| diagnostics::exec(db, source_unit_id))
    }

    pub fn document_symbol(&self, file_id: FileId) -> Cancellable<Option<Vec<DocumentSymbol>>> {
        self.with_db(|db| document_symbol::exec(db, file_id))
    }

    pub fn goto_definition(
        &self,
        source_unit_id: SourceUnitId,
        pos: FilePosition,
    ) -> Cancellable<Option<FileRange>> {
        self.with_db(|db| goto_definition::exec(db, source_unit_id, pos))
    }

    pub fn references(
        &self,
        source_unit_id: SourceUnitId,
        pos: FilePosition,
    ) -> Cancellable<Option<Vec<FileRange>>> {
        self.with_db(|db| references::exec(db, source_unit_id, pos))
    }

    pub fn hover(
        &self,
        source_unit_id: SourceUnitId,
        pos: FilePosition,
    ) -> Cancellable<Option<Hover>> {
        self.with_db(|db| hover::exec(db, source_unit_id, pos))
    }

    pub fn inlay_hint(
        &self,
        source_unit_id: SourceUnitId,
        range: FileRange,
    ) -> Cancellable<Option<Vec<InlayHint>>> {
        self.with_db(|db| inlay_hint::exec(db, source_unit_id, range))
    }

    pub fn completion(
        &self,
        source_unit_id: SourceUnitId,
        pos: FilePosition,
        trigger_char: Option<String>,
    ) -> Cancellable<Option<Vec<CompletionItem>>> {
        self.with_db(|db| completion::exec(db, source_unit_id, pos, trigger_char))
    }

    pub fn completion_with_index(
        &self,
        pos: FilePosition,
        trigger_char: Option<String>,
        index: Arc<Index>,
    ) -> Cancellable<Option<Vec<CompletionItem>>> {
        self.with_db(|db| completion::exec_with_index(db, pos, trigger_char, &index))
    }

    pub fn document_link(
        &self,
        source_unit_id: SourceUnitId,
        file_id: FileId,
    ) -> Cancellable<Option<Vec<DocumentLink>>> {
        self.with_db(|db| document_link::exec(db, source_unit_id, file_id))
    }

    pub fn folding_range(&self, file_id: FileId) -> Cancellable<Option<Vec<FoldingRange>>> {
        self.with_db(|db| folding_range::exec(db, file_id))
    }
}
