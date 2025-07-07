use std::sync::Arc;

use dashmap::{DashMap, Entry};
use salsa::Setter;
use syntax::Parse;

use crate::file_system::{FileId, SourceUnit, SourceUnitId};
use crate::line_index::LineIndex;

#[salsa::db]
#[derive(Clone, Default)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
    files: Arc<Files>,
}

#[salsa::db]
impl salsa::Database for RootDatabase {}

#[salsa::db]
pub trait Db: salsa::Database {
    fn set_file_content(&mut self, file_id: FileId, content: &str);

    fn file_content(&self, file_id: FileId) -> FileContent;

    fn set_source_unit(&mut self, source_unit_id: SourceUnitId, source_unit: SourceUnit);

    fn source_unit(&self, source_unit_id: SourceUnitId) -> Arc<SourceUnit>;
}

#[salsa::input]
pub struct FileContent {
    pub file_id: FileId,
    pub content: Arc<str>,
}

#[salsa::db]
impl Db for RootDatabase {
    fn set_file_content(&mut self, file_id: FileId, content: &str) {
        let files = Arc::clone(&self.files);
        files.set_file_content(self, file_id, content);
    }

    fn file_content(&self, file_id: FileId) -> FileContent {
        self.files.file_content(file_id)
    }

    fn set_source_unit(&mut self, source_unit_id: SourceUnitId, source_unit: SourceUnit) {
        let files = Arc::clone(&self.files);
        files.set_source_unit(source_unit_id, source_unit);
    }

    fn source_unit(&self, source_unit_id: SourceUnitId) -> Arc<SourceUnit> {
        self.files.source_unit(source_unit_id)
    }
}

#[derive(Default)]
pub struct Files {
    file_contents: Arc<DashMap<FileId, FileContent>>,
    source_units: Arc<DashMap<SourceUnitId, Arc<SourceUnit>>>,
}

impl Files {
    fn set_file_content(&self, db: &mut dyn Db, file_id: FileId, content: &str) {
        match self.file_contents.entry(file_id) {
            Entry::Vacant(vacant) => {
                let file_content = FileContent::new(db, file_id, Arc::from(content));
                vacant.insert(file_content);
            }
            Entry::Occupied(mut occupied) => {
                occupied.get_mut().set_content(db).to(Arc::from(content));
            }
        }
    }

    fn file_content(&self, file_id: FileId) -> FileContent {
        *self
            .file_contents
            .get(&file_id)
            .expect("File content not found")
    }

    fn set_source_unit(&self, source_unit_id: SourceUnitId, source_unit: SourceUnit) {
        self.source_units
            .insert(source_unit_id, Arc::new(source_unit));
    }

    fn source_unit(&self, source_unit_id: SourceUnitId) -> Arc<SourceUnit> {
        self.source_units
            .get(&source_unit_id)
            .expect("Source unit not found")
            .clone()
    }
}

#[salsa::input]
struct File {
    id: FileId,
}

pub fn line_index(db: &dyn Db, file_id: FileId) -> Arc<LineIndex> {
    #[salsa::tracked]
    fn line_index(db: &dyn Db, file: File) -> Arc<LineIndex> {
        let file_content = db.file_content(file.id(db));
        let content = file_content.content(db);
        Arc::new(LineIndex::new(&content))
    }
    line_index(db, File::new(db, file_id))
}

pub fn parse(db: &dyn Db, file_id: FileId) -> Parse {
    #[salsa::tracked]
    fn parse(db: &dyn Db, file: File) -> Parse {
        let file_content = db.file_content(file.id(db));
        let content = file_content.content(db);
        syntax::parse(&content)
    }
    parse(db, File::new(db, file_id))
}
