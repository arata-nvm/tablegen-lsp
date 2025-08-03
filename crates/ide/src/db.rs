use std::sync::Arc;

use syntax::Parse;

use crate::file_system::{FileId, SourceUnit, SourceUnitId};
use crate::index::IndexDatabaseStorage;
use crate::interop::{TblgenDiagnostic, TblgenSymbolTable};
use crate::line_index::LineIndex;

#[salsa::database(SourceDatabaseStorage, IndexDatabaseStorage)]
#[derive(Default)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for RootDatabase {}

impl salsa::ParallelDatabase for RootDatabase {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(RootDatabase {
            storage: self.storage.snapshot(),
        })
    }
}

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase {
    #[salsa::input]
    fn file_content(&self, file_id: FileId) -> Arc<str>;

    #[salsa::input]
    fn source_unit(&self, source_unit_id: SourceUnitId) -> Arc<SourceUnit>;

    #[salsa::input]
    fn tblgen_diagnostics(
        &self,
        source_unit_id: SourceUnitId,
    ) -> Option<Arc<Vec<TblgenDiagnostic>>>;

    #[salsa::input]
    fn tblgen_symbol_table(&self, source_unit_id: SourceUnitId) -> Option<Arc<TblgenSymbolTable>>;

    fn line_index(&self, file_id: FileId) -> Arc<LineIndex>;

    fn parse(&self, file_id: FileId) -> Parse;
}

fn line_index(db: &dyn SourceDatabase, file_id: FileId) -> Arc<LineIndex> {
    let text = db.file_content(file_id);
    Arc::new(LineIndex::new(&text))
}

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> Parse {
    let text = db.file_content(file_id);
    syntax::parse(&text)
}
