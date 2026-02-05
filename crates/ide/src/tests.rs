use std::{collections::HashMap, path::Path, sync::Arc};

use salsa::Setter as _;
use syntax::parser::TextRange;

use crate::{
    db::{SetFileContent, SetSourceUnit, SetTblgenDiagnostics, SetTblgenSymbolTable, SourceDatabase},
    file_system::{
        self, FileId, FilePath, FilePosition, FileRange, FileSet, FileSystem, SourceUnitId,
    },
    index::IndexDatabase,
    interop,
};

const DEFAULT_FILE_PATH: &str = "/main.td";
const MARKER_INDICATOR: char = '$';

pub fn single_file(fixture: &str) -> (TestDatabase, Fixture) {
    let mut f = Fixture::single_file(fixture);
    let db = TestDatabase::new(&mut f);
    (db, f)
}

pub fn multiple_files(fixture: &str) -> (TestDatabase, Fixture) {
    let mut f = Fixture::multiple_files(fixture);
    let db = TestDatabase::new(&mut f);
    (db, f)
}

pub fn load_single_file(path: &str) -> (TestDatabase, Fixture) {
    let mut f = Fixture::load_single_file(path);
    let db = TestDatabase::new(&mut f);
    (db, f)
}

pub fn load_single_file_with_tblgen(path: &str) -> (TestDatabase, Fixture) {
    let mut f = Fixture::load_single_file(path);
    let db = TestDatabase::new_with_tblgen(&mut f);
    (db, f)
}

#[salsa::db]
#[derive(Clone, Default)]
pub struct TestDatabase {
    storage: salsa::Storage<Self>,
    file_contents: std::collections::HashMap<FileId, crate::db::FileContent>,
    source_units: std::collections::HashMap<SourceUnitId, crate::db::SourceUnitInput>,
    tblgen_diagnostics_map: std::collections::HashMap<SourceUnitId, crate::db::TblgenDiagnostics>,
    tblgen_symbol_tables: std::collections::HashMap<SourceUnitId, crate::db::TblgenSymbolTableInput>,
}

#[salsa::db]
impl salsa::Database for TestDatabase {}

#[salsa::db]
impl SourceDatabase for TestDatabase {
    fn file_content(&self, file_id: FileId) -> Arc<str> {
        self.file_contents
            .get(&file_id)
            .map(|fc| fc.content(self).clone())
            .unwrap_or_else(|| Arc::from(""))
    }

    fn source_unit(&self, source_unit_id: SourceUnitId) -> Arc<crate::file_system::SourceUnit> {
        self.source_units
            .get(&source_unit_id)
            .map(|su| su.source_unit(self).clone())
            .unwrap_or_else(|| {
                use std::collections::HashMap;
                Arc::new(crate::file_system::SourceUnit {
                    root: source_unit_id.root_file(),
                    includes: HashMap::new(),
                })
            })
    }

    fn tblgen_diagnostics(
        &self,
        source_unit_id: SourceUnitId,
    ) -> Option<Arc<Vec<crate::interop::TblgenDiagnostic>>> {
        self.tblgen_diagnostics_map
            .get(&source_unit_id)
            .and_then(|td| td.diagnostics(self))
    }

    fn tblgen_symbol_table(
        &self,
        source_unit_id: SourceUnitId,
    ) -> Option<Arc<crate::interop::TblgenSymbolTable>> {
        self.tblgen_symbol_tables
            .get(&source_unit_id)
            .and_then(|ts| ts.symbol_table(self))
    }
}

#[salsa::db]
impl IndexDatabase for TestDatabase {
    fn index(&self, source_unit_id: SourceUnitId) -> Arc<crate::index::Index> {
        let source_unit_id_interned = crate::index::SourceUnitIdInterned::new(self, source_unit_id.root_file().0);
        crate::index::index(self, source_unit_id_interned)
    }
}

impl crate::db::ParseDatabase for TestDatabase {
    fn parse(&self, file_id: FileId) -> syntax::Parse {
        let file_id_interned = crate::db::FileIdInterned::new(self, file_id.0);
        crate::db::parse(self, file_id_interned)
    }
}

impl SetFileContent for TestDatabase {
    fn set_file_content(&mut self, file_id: FileId, content: Arc<str>) {
        if !self.file_contents.contains_key(&file_id) {
            let file_content = crate::db::FileContent::new(self, content.clone());
            self.file_contents.insert(file_id, file_content);
        }
        if let Some(file_content) = self.file_contents.get_mut(&file_id) {
            file_content.set_content(self).to(content);
        }
    }
}

impl SetSourceUnit for TestDatabase {
    fn set_source_unit(&mut self, source_unit_id: SourceUnitId, source_unit: Arc<crate::file_system::SourceUnit>) {
        if !self.source_units.contains_key(&source_unit_id) {
            let su_input = crate::db::SourceUnitInput::new(self, source_unit.clone());
            self.source_units.insert(source_unit_id, su_input);
        }
        if let Some(su_input) = self.source_units.get_mut(&source_unit_id) {
            su_input.set_source_unit(self).to(source_unit);
        }
    }
}

impl SetTblgenDiagnostics for TestDatabase {
    fn set_tblgen_diagnostics(
        &mut self,
        source_unit_id: SourceUnitId,
        diagnostics: Option<Arc<Vec<crate::interop::TblgenDiagnostic>>>,
    ) {
        if !self.tblgen_diagnostics_map.contains_key(&source_unit_id) {
            let td_input = crate::db::TblgenDiagnostics::new(self, diagnostics.clone());
            self.tblgen_diagnostics_map.insert(source_unit_id, td_input);
        }
        if let Some(td_input) = self.tblgen_diagnostics_map.get_mut(&source_unit_id) {
            td_input.set_diagnostics(self).to(diagnostics);
        }
    }
}

impl SetTblgenSymbolTable for TestDatabase {
    fn set_tblgen_symbol_table(
        &mut self,
        source_unit_id: SourceUnitId,
        symbol_table: Option<Arc<crate::interop::TblgenSymbolTable>>,
    ) {
        if !self.tblgen_symbol_tables.contains_key(&source_unit_id) {
            let ts_input = crate::db::TblgenSymbolTableInput::new(self, symbol_table.clone());
            self.tblgen_symbol_tables.insert(source_unit_id, ts_input);
        }
        if let Some(ts_input) = self.tblgen_symbol_tables.get_mut(&source_unit_id) {
            ts_input.set_symbol_table(self).to(symbol_table);
        }
    }
}

impl TestDatabase {
    fn new(f: &mut Fixture) -> Self {
        let mut db = Self::default();
        for (file_id, content) in f.files() {
            db.set_file_content(file_id, Arc::from(content));
        }

        let id = SourceUnitId::from_root_file(f.root_file());
        let source_unit = file_system::collect_sources(&mut db, f, f.root_file(), &[]);
        db.set_source_unit(id, Arc::new(source_unit));
        db.set_tblgen_diagnostics(id, None);
        db.set_tblgen_symbol_table(id, None);

        db
    }

    fn new_with_tblgen(f: &mut Fixture) -> Self {
        let mut db = Self::new(f);
        let root_file_id = f.root_file();
        let root_file_path = f.path_for_file(&root_file_id);

        let result = interop::parse_source_unit_with_tblgen(&root_file_path, &[], f)
            .expect("failed to parse source unit with tblgen");
        let source_unit_id = SourceUnitId::from_root_file(root_file_id);
        db.set_tblgen_diagnostics(source_unit_id, Some(Arc::new(result.diagnostics)));
        db.set_tblgen_symbol_table(source_unit_id, Some(Arc::new(result.symbol_table)));
        db
    }
}

#[derive(Debug, Default)]
pub struct Fixture {
    file_contents: HashMap<FilePath, String>,
    file_ids: Vec<FileId>,
    markers: Vec<FilePosition>,

    file_set: FileSet,
    next_file_id: u32,
}

impl Fixture {
    fn single_file(fixture: &str) -> Self {
        let mut this = Self::default();
        let path = FilePath::from(Path::new(DEFAULT_FILE_PATH));
        let content = this.parse_file(&mut fixture.lines().peekable());
        this.insert_file(path, content);
        this
    }

    fn multiple_files(fixture: &str) -> Self {
        let mut this = Self::default();

        let mut lines = fixture
            .lines()
            .skip_while(|line| !line.starts_with("; "))
            .peekable();
        while let Some(line) = lines.next() {
            let path_str = line
                .strip_prefix("; ")
                .expect("expected header line to start with '; '");
            let path = FilePath::from(Path::new(path_str));
            let content = this.parse_file(&mut lines);
            this.insert_file(path, content);
        }

        this
    }

    fn load_single_file(path: &str) -> Self {
        let content = std::fs::read_to_string(path).expect("failed to read fixture file");

        let mut this = Self::default();
        let path = FilePath::from(Path::new(path));
        let content = this.parse_file(&mut content.lines().peekable());
        this.insert_file(path, content);
        this
    }

    fn parse_file<'a>(
        &mut self,
        lines: &mut std::iter::Peekable<impl Iterator<Item = &'a str>>,
    ) -> String {
        let mut content = String::new();
        while let Some(line) = lines.peek() {
            if line.starts_with("; ") {
                break;
            }
            if !content.is_empty() {
                content.push('\n');
            }

            let line = lines.next().expect("line must be present");
            for c in line.chars() {
                if c != MARKER_INDICATOR {
                    content.push(c);
                } else {
                    let marker =
                        FilePosition::new(self.next_file_id(), content.len().try_into().unwrap());
                    self.markers.push(marker);
                    continue;
                }
            }
        }
        content
    }

    fn files(&self) -> Vec<(FileId, &str)> {
        self.file_contents
            .iter()
            .map(|(path, content)| {
                (
                    self.file_set
                        .file_for_path(path)
                        .expect("file id must exist"),
                    content.as_str(),
                )
            })
            .collect()
    }

    pub fn root_file(&self) -> FileId {
        *self
            .file_ids
            .first()
            .expect("at least one file must be exist")
    }

    pub fn marker(&self, index: usize) -> FilePosition {
        self.markers[index]
    }

    pub fn full_range(&self, file: FileId) -> FileRange {
        FileRange {
            file,
            range: TextRange::new(0.into(), self.file_content(&file).len().try_into().unwrap()),
        }
    }

    pub fn file_content(&self, id: &FileId) -> String {
        let path = self.file_set.path_for_file(id);
        self.file_contents[&path].clone()
    }

    pub fn source_unit_id(&self) -> SourceUnitId {
        SourceUnitId::from_root_file(self.root_file())
    }

    fn insert_file(&mut self, path: FilePath, content: String) {
        let res = self.file_contents.insert(path.clone(), content);
        assert!(res.is_none(), "duplicate file path");

        let file_id = self.alloc_file_id();
        self.file_set.insert(file_id, path);
        self.file_ids.push(file_id);
    }

    fn alloc_file_id(&mut self) -> FileId {
        let file_id = FileId(self.next_file_id);
        self.next_file_id += 1;
        file_id
    }

    fn next_file_id(&mut self) -> FileId {
        FileId(self.next_file_id)
    }
}

impl FileSystem for Fixture {
    fn assign_or_get_file_id(&mut self, path: FilePath) -> FileId {
        match self.file_set.file_for_path(&path) {
            Some(file_id) => file_id,
            None => {
                let file_id = self.alloc_file_id();
                self.file_set.insert(file_id, path);
                file_id
            }
        }
    }

    fn file_for_path(&self, path: &FilePath) -> Option<FileId> {
        self.file_set.file_for_path(path)
    }

    fn path_for_file(&self, file_id: &FileId) -> FilePath {
        self.file_set.path_for_file(file_id)
    }

    fn read_content(&self, file_path: &FilePath) -> Option<String> {
        self.file_contents.get(file_path).cloned()
    }
}
