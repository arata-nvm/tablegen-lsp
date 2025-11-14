use std::sync::Arc;

use salsa::ParallelDatabase;

use crate::db::{RootDatabase, SourceDatabase};
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
use crate::index::{Index, IndexDatabase};
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
            db: self.db.snapshot(),
        }
    }

    pub fn set_file_content(&mut self, file_id: FileId, text: Arc<str>) {
        self.db.set_file_content(file_id, text);
    }

    pub fn set_tblgen_parse_result(
        &mut self,
        source_unit_id: SourceUnitId,
        result: TblgenParseResult,
    ) {
        self.db
            .set_tblgen_diagnostics(source_unit_id, Some(Arc::new(result.diagnostics)));
        self.db
            .set_tblgen_symbol_table(source_unit_id, Some(Arc::new(result.symbol_table)));
    }

    pub fn load_source_unit<FS: FileSystem>(
        &mut self,
        fs: &mut FS,
        root_file: FileId,
        include_dirs: &[FilePath],
    ) -> SourceUnitId {
        let id = SourceUnitId::from_root_file(root_file);
        let source_unit = file_system::collect_sources(&mut self.db, fs, root_file, include_dirs);
        self.db.set_source_unit(id, Arc::new(source_unit));
        self.db.set_tblgen_diagnostics(id, None);
        self.db.set_tblgen_symbol_table(id, None);
        id
    }
}

pub struct Analysis {
    db: salsa::Snapshot<RootDatabase>,
}

impl Analysis {
    pub fn line_index(&self, file_id: FileId) -> Arc<LineIndex> {
        self.db.line_index(file_id)
    }

    pub fn index(&self, source_unit_id: SourceUnitId) -> Arc<Index> {
        self.db.index(source_unit_id)
    }

    pub fn source_unit(&self, source_unit_id: SourceUnitId) -> Arc<SourceUnit> {
        self.db.source_unit(source_unit_id)
    }

    pub fn diagnostics(&self, source_unit_id: SourceUnitId) -> Vec<Diagnostic> {
        diagnostics::exec(&*self.db, source_unit_id)
    }

    pub fn document_symbol(&self, file_id: FileId) -> Option<Vec<DocumentSymbol>> {
        document_symbol::exec(&*self.db, file_id)
    }

    pub fn goto_definition(
        &self,
        source_unit_id: SourceUnitId,
        pos: FilePosition,
    ) -> Option<FileRange> {
        goto_definition::exec(&*self.db, source_unit_id, pos)
    }

    pub fn references(
        &self,
        source_unit_id: SourceUnitId,
        pos: FilePosition,
    ) -> Option<Vec<FileRange>> {
        references::exec(&*self.db, source_unit_id, pos)
    }

    pub fn hover(&self, source_unit_id: SourceUnitId, pos: FilePosition) -> Option<Hover> {
        hover::exec(&*self.db, source_unit_id, pos)
    }

    pub fn inlay_hint(
        &self,
        source_unit_id: SourceUnitId,
        range: FileRange,
    ) -> Option<Vec<InlayHint>> {
        inlay_hint::exec(&*self.db, source_unit_id, range)
    }

    pub fn completion(
        &self,
        source_unit_id: SourceUnitId,
        pos: FilePosition,
        trigger_char: Option<String>,
    ) -> Option<Vec<CompletionItem>> {
        completion::exec(&*self.db, source_unit_id, pos, trigger_char)
    }

    pub fn document_link(
        &self,
        source_unit_id: SourceUnitId,
        file_id: FileId,
    ) -> Option<Vec<DocumentLink>> {
        document_link::exec(&*self.db, source_unit_id, file_id)
    }

    pub fn folding_range(&self, file_id: FileId) -> Option<Vec<FoldingRange>> {
        folding_range::exec(&*self.db, file_id)
    }
}
