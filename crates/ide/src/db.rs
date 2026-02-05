use std::sync::Arc;

use salsa::Setter as _;

use crate::file_system::{FileId, SourceUnit, SourceUnitId};
use crate::index::IndexDatabase;
use crate::interop::{TblgenDiagnostic, TblgenSymbolTable};
use crate::line_index::LineIndex;

#[salsa::input]
pub struct FileContent {
    #[returns(ref)]
    pub content: Arc<str>,
}

#[salsa::input]
pub struct SourceUnitInput {
    #[returns(ref)]
    pub source_unit: Arc<SourceUnit>,
}

#[salsa::input]
pub struct TblgenDiagnostics {
    pub diagnostics: Option<Arc<Vec<TblgenDiagnostic>>>,
}

#[salsa::input]
pub struct TblgenSymbolTableInput {
    pub symbol_table: Option<Arc<TblgenSymbolTable>>,
}

pub trait SetFileContent {
    fn set_file_content(&mut self, file_id: FileId, content: Arc<str>);
}

pub trait SetSourceUnit {
    fn set_source_unit(&mut self, source_unit_id: SourceUnitId, source_unit: Arc<SourceUnit>);
}

pub trait SetTblgenDiagnostics {
    fn set_tblgen_diagnostics(
        &mut self,
        source_unit_id: SourceUnitId,
        diagnostics: Option<Arc<Vec<TblgenDiagnostic>>>,
    );
}

pub trait SetTblgenSymbolTable {
    fn set_tblgen_symbol_table(
        &mut self,
        source_unit_id: SourceUnitId,
        symbol_table: Option<Arc<TblgenSymbolTable>>,
    );
}

pub trait ParseDatabase {
    fn parse(&self, file_id: FileId) -> syntax::Parse;
}

#[salsa::db]
pub trait SourceDatabase: salsa::Database {
    fn file_content(&self, file_id: FileId) -> Arc<str>;
    fn source_unit(&self, source_unit_id: SourceUnitId) -> Arc<SourceUnit>;
    fn tblgen_diagnostics(
        &self,
        source_unit_id: SourceUnitId,
    ) -> Option<Arc<Vec<TblgenDiagnostic>>>;
    fn tblgen_symbol_table(&self, source_unit_id: SourceUnitId) -> Option<Arc<TblgenSymbolTable>>;
    fn line_index(&self, file_id: FileId) -> Arc<LineIndex>
    where
        Self: Sized,
    {
        let file_id_interned = FileIdInterned::new(self, file_id.0);
        line_index(self as &dyn SourceDatabase, file_id_interned)
    }
}

#[salsa::interned]
pub struct FileIdInterned {
    id: u32,
}

#[salsa::tracked]
fn line_index<'db>(db: &'db dyn SourceDatabase, file_id: FileIdInterned<'db>) -> Arc<LineIndex> {
    let file_id_value = FileId(file_id.id(db));
    let text = db.file_content(file_id_value);
    Arc::new(LineIndex::new(&text))
}

#[salsa::tracked]
pub fn parse<'db>(db: &'db dyn SourceDatabase, file_id: FileIdInterned<'db>) -> syntax::Parse {
    let file_id_value = FileId(file_id.id(db));
    let text = db.file_content(file_id_value);
    syntax::parse(&text)
}

#[salsa::db]
#[derive(Clone, Default)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
    file_contents: std::collections::HashMap<FileId, FileContent>,
    source_units: std::collections::HashMap<SourceUnitId, SourceUnitInput>,
    tblgen_diagnostics_map: std::collections::HashMap<SourceUnitId, TblgenDiagnostics>,
    tblgen_symbol_tables: std::collections::HashMap<SourceUnitId, TblgenSymbolTableInput>,
}

#[salsa::db]
impl salsa::Database for RootDatabase {}

#[salsa::db]
impl SourceDatabase for RootDatabase {
    fn file_content(&self, file_id: FileId) -> Arc<str> {
        self.file_contents
            .get(&file_id)
            .map(|fc| fc.content(self).clone())
            .unwrap_or_else(|| Arc::from(""))
    }

    fn source_unit(&self, source_unit_id: SourceUnitId) -> Arc<SourceUnit> {
        self.source_units
            .get(&source_unit_id)
            .map(|su| su.source_unit(self).clone())
            .unwrap_or_else(|| {
                // Create a minimal SourceUnit if not found
                // Use collect_sources with an empty file system to create a minimal SourceUnit
                use std::collections::HashMap;
                let root = source_unit_id.root_file();
                Arc::new(SourceUnit {
                    root,
                    includes: HashMap::new(),
                })
            })
    }

    fn tblgen_diagnostics(
        &self,
        source_unit_id: SourceUnitId,
    ) -> Option<Arc<Vec<TblgenDiagnostic>>> {
        self.tblgen_diagnostics_map
            .get(&source_unit_id)
            .and_then(|td| td.diagnostics(self))
    }

    fn tblgen_symbol_table(&self, source_unit_id: SourceUnitId) -> Option<Arc<TblgenSymbolTable>> {
        self.tblgen_symbol_tables
            .get(&source_unit_id)
            .and_then(|ts| ts.symbol_table(self))
    }
}

impl SetFileContent for RootDatabase {
    fn set_file_content(&mut self, file_id: FileId, content: Arc<str>) {
        if !self.file_contents.contains_key(&file_id) {
            let file_content = FileContent::new(self, content.clone());
            self.file_contents.insert(file_id, file_content);
        }
        if let Some(file_content) = self.file_contents.get_mut(&file_id) {
            file_content.set_content(self).to(content);
        }
    }
}

impl SetSourceUnit for RootDatabase {
    fn set_source_unit(&mut self, source_unit_id: SourceUnitId, source_unit: Arc<SourceUnit>) {
        if !self.source_units.contains_key(&source_unit_id) {
            let su_input = SourceUnitInput::new(self, source_unit.clone());
            self.source_units.insert(source_unit_id, su_input);
        }
        if let Some(su_input) = self.source_units.get_mut(&source_unit_id) {
            su_input.set_source_unit(self).to(source_unit);
        }
    }
}

impl SetTblgenDiagnostics for RootDatabase {
    fn set_tblgen_diagnostics(
        &mut self,
        source_unit_id: SourceUnitId,
        diagnostics: Option<Arc<Vec<TblgenDiagnostic>>>,
    ) {
        if !self.tblgen_diagnostics_map.contains_key(&source_unit_id) {
            let td_input = TblgenDiagnostics::new(self, diagnostics.clone());
            self.tblgen_diagnostics_map.insert(source_unit_id, td_input);
        }
        if let Some(td_input) = self.tblgen_diagnostics_map.get_mut(&source_unit_id) {
            td_input.set_diagnostics(self).to(diagnostics);
        }
    }
}

impl SetTblgenSymbolTable for RootDatabase {
    fn set_tblgen_symbol_table(
        &mut self,
        source_unit_id: SourceUnitId,
        symbol_table: Option<Arc<TblgenSymbolTable>>,
    ) {
        if !self.tblgen_symbol_tables.contains_key(&source_unit_id) {
            let ts_input = TblgenSymbolTableInput::new(self, symbol_table.clone());
            self.tblgen_symbol_tables.insert(source_unit_id, ts_input);
        }
        if let Some(ts_input) = self.tblgen_symbol_tables.get_mut(&source_unit_id) {
            ts_input.set_symbol_table(self).to(symbol_table);
        }
    }
}

impl RootDatabase {
    pub fn snapshot(&self) -> Self {
        Self {
            storage: self.storage.clone(),
            file_contents: self.file_contents.clone(),
            source_units: self.source_units.clone(),
            tblgen_diagnostics_map: self.tblgen_diagnostics_map.clone(),
            tblgen_symbol_tables: self.tblgen_symbol_tables.clone(),
        }
    }
}

#[salsa::db]
impl IndexDatabase for RootDatabase {
    fn index(&self, source_unit_id: SourceUnitId) -> Arc<crate::index::Index> {
        let source_unit_id_interned =
            crate::index::SourceUnitIdInterned::new(self, source_unit_id.root_file().0);
        crate::index::index(self, source_unit_id_interned)
    }
}

impl ParseDatabase for RootDatabase {
    fn parse(&self, file_id: FileId) -> syntax::Parse {
        let file_id_interned = FileIdInterned::new(self, file_id.0);
        parse(self, file_id_interned)
    }
}
