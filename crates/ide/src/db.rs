use std::sync::Arc;

use dashmap::{DashMap, Entry};
use salsa::Setter;
use syntax::Parse;

use crate::{
    file_system::{FileId, SourceUnit, SourceUnitId},
    index::Index,
    interop::{TblgenDiagnostic, TblgenParseResult, TblgenSymbolTable},
    line_index::LineIndex,
};

#[salsa::input]
pub struct FileInput {
    #[returns(ref)]
    pub content: Arc<str>,
}

#[salsa::input]
pub struct SourceUnitInput {
    pub id: SourceUnitId,
    #[returns(ref)]
    pub source_unit: Arc<SourceUnit>,
}

#[salsa::input]
pub struct TblgenResultInput {
    #[returns(ref)]
    pub diagnostics: Vec<TblgenDiagnostic>,
    #[returns(ref)]
    pub symbol_table: TblgenSymbolTable,
}

#[salsa::db]
pub trait Database: salsa::Database {
    fn file(&self, id: FileId) -> FileInput;

    fn set_file(&mut self, id: FileId, content: Arc<str>);

    fn source_unit(&self, id: SourceUnitId) -> SourceUnitInput;

    fn set_source_unit(&mut self, id: SourceUnitId, source_unit: Arc<SourceUnit>);

    fn tblgen_result(&self, id: SourceUnitId) -> Option<TblgenResultInput>;

    fn set_tblgen_result(&mut self, id: SourceUnitId, result: TblgenParseResult);
}

#[salsa::db]
pub trait SourceDatabase: Database {
    fn line_index(&self, id: FileId) -> Arc<LineIndex>;

    fn parse(&self, file_id: FileId) -> Arc<Parse>;
}

#[salsa::db]
pub trait IndexDatabase: SourceDatabase {
    fn index(&self, id: SourceUnitId) -> Arc<Index>;
}

#[salsa::db]
#[derive(Clone, Default)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
    files: Arc<DashMap<FileId, FileInput>>,
    source_units: Arc<DashMap<SourceUnitId, SourceUnitInput>>,
    tblgen_results: Arc<DashMap<SourceUnitId, TblgenResultInput>>,
}

#[salsa::db]
impl salsa::Database for RootDatabase {}

#[salsa::db]
impl Database for RootDatabase {
    fn file(&self, id: FileId) -> FileInput {
        *self.files.get(&id).expect("file not found")
    }

    fn set_file(&mut self, id: FileId, content: Arc<str>) {
        let files = Arc::clone(&self.files);
        match files.entry(id) {
            Entry::Vacant(entry) => {
                let input = FileInput::new(self, content);
                entry.insert(input);
            }
            Entry::Occupied(mut entry) => {
                entry.get_mut().set_content(self).to(content);
            }
        }
    }

    fn source_unit(&self, id: SourceUnitId) -> SourceUnitInput {
        *self.source_units.get(&id).expect("source unit not found")
    }

    fn set_source_unit(&mut self, id: SourceUnitId, source_unit: Arc<SourceUnit>) {
        let input = SourceUnitInput::new(self, id, source_unit);
        self.source_units.insert(id, input);
    }

    fn tblgen_result(&self, id: SourceUnitId) -> Option<TblgenResultInput> {
        self.tblgen_results.get(&id).as_deref().copied()
    }

    fn set_tblgen_result(&mut self, id: SourceUnitId, result: TblgenParseResult) {
        let input = TblgenResultInput::new(self, result.diagnostics, result.symbol_table);
        self.tblgen_results.insert(id, input);
    }
}

#[salsa::db]
impl SourceDatabase for RootDatabase {
    fn line_index(&self, id: FileId) -> Arc<LineIndex> {
        line_index(self, self.file(id))
    }

    fn parse(&self, file_id: FileId) -> Arc<Parse> {
        parse(self, self.file(file_id))
    }
}

#[salsa::tracked]
fn line_index(db: &dyn SourceDatabase, file: FileInput) -> Arc<LineIndex> {
    let content = file.content(db);
    Arc::new(LineIndex::new(content))
}

#[salsa::tracked]
fn parse(db: &dyn SourceDatabase, file: FileInput) -> Arc<Parse> {
    let content = file.content(db);
    Arc::new(syntax::parse(content))
}

#[salsa::db]
impl IndexDatabase for RootDatabase {
    fn index(&self, id: SourceUnitId) -> Arc<Index> {
        index(self, self.source_unit(id))
    }
}

#[salsa::tracked]
fn index(db: &dyn IndexDatabase, source_unit: SourceUnitInput) -> Arc<Index> {
    Arc::new(crate::index::index(db, source_unit.id(db)))
}
