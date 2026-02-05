use std::collections::HashSet;
use std::sync::Arc;

use salsa::Cancelled;

use crate::Cancellable;
use crate::db::{
    RootDatabase, SetFileContent, SetSourceUnit, SetTblgenDiagnostics, SetTblgenSymbolTable,
    SourceDatabase,
};
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

pub struct AnalysisHost {
    db: RootDatabase,
    initialized_tblgen_units: HashSet<SourceUnitId>,
}

impl Default for AnalysisHost {
    fn default() -> Self {
        Self {
            db: RootDatabase::default(),
            initialized_tblgen_units: HashSet::new(),
        }
    }
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

        // 存在しないデータにアクセスを試みるとクラッシュするので、初回のみNoneで初期化する
        if !self.initialized_tblgen_units.contains(&id) {
            self.db.set_tblgen_diagnostics(id, None);
            self.db.set_tblgen_symbol_table(id, None);
            self.initialized_tblgen_units.insert(id);
        }
        id
    }
}

pub struct Analysis {
    db: RootDatabase,
}

impl Analysis {
    fn with_db<F, T>(&self, f: F) -> Cancellable<T>
    where
        F: FnOnce(&RootDatabase) -> T + std::panic::UnwindSafe,
    {
        Cancelled::catch(|| f(&self.db))
    }

    pub fn line_index(&self, file_id: FileId) -> Cancellable<Arc<LineIndex>> {
        self.with_db(|db| db.line_index(file_id))
    }

    pub fn index(&self, source_unit_id: SourceUnitId) -> Cancellable<Arc<Index>> {
        self.with_db(|db| db.index(source_unit_id))
    }

    pub fn source_unit(&self, source_unit_id: SourceUnitId) -> Cancellable<Arc<SourceUnit>> {
        self.with_db(|db| db.source_unit(source_unit_id))
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
