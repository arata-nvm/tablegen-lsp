use std::{collections::HashMap, path::Path, sync::Arc};

use syntax::parser::TextRange;

use crate::{
    db::{SourceDatabase, SourceDatabaseStorage},
    file_system::{self, FileId, FilePath, FilePosition, FileRange, FileSet, FileSystem},
    index::IndexDatabaseStorage,
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

#[salsa::database(SourceDatabaseStorage, IndexDatabaseStorage)]
#[derive(Default)]
pub struct TestDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for TestDatabase {}

impl TestDatabase {
    fn new(f: &mut Fixture) -> Self {
        let mut db = Self::default();
        for (file_id, content) in f.files() {
            db.set_file_content(file_id, Arc::from(content));
        }

        let source_root = file_system::collect_sources(&mut db, f, f.root_file());
        db.set_source_root(Arc::new(source_root));

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
        self.file_contents[path].clone()
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

    fn path_for_file(&self, file_id: &FileId) -> &FilePath {
        self.file_set.path_for_file(file_id)
    }

    fn read_content(&self, file_path: &FilePath) -> Option<String> {
        self.file_contents.get(file_path).cloned()
    }
}
