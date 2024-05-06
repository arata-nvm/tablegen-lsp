use std::collections::{HashMap, VecDeque};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use syntax::ast::AstNode;
use syntax::{ast, SyntaxNode};

use crate::db::{RootDatabase, SourceDatabase};

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct FileId(pub u32);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FilePath(pub PathBuf);

impl FilePath {
    pub fn join(&self, path: impl AsRef<Path>) -> FilePath {
        FilePath(self.0.join(path))
    }

    pub fn parent(&self) -> Option<FilePath> {
        self.0.parent().map(FilePath::from)
    }
}

impl From<&Path> for FilePath {
    fn from(value: &Path) -> Self {
        Self(value.to_path_buf())
    }
}

#[derive(Debug, Default)]
pub struct FileSet {
    path_to_id: HashMap<FilePath, FileId>,
    id_to_path: HashMap<FileId, FilePath>,
}

impl FileSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, file_id: FileId, path: FilePath) {
        self.path_to_id.insert(path.clone(), file_id);
        self.id_to_path.insert(file_id, path.clone());
    }

    pub fn remove(&mut self, file_id: &FileId) {
        if let Some(path) = self.id_to_path.remove(file_id) {
            self.path_to_id.remove(&path);
        }
    }

    pub fn file_for_path(&self, path: &FilePath) -> Option<FileId> {
        self.path_to_id.get(path).copied()
    }

    pub fn path_for_file(&self, file_id: &FileId) -> Option<FilePath> {
        self.id_to_path.get(file_id).cloned()
    }
}

#[derive(Debug)]
pub struct SourceRoot {
    file_set: FileSet,
    root: FileId,
}

impl SourceRoot {
    pub fn new(file_set: FileSet, root: FileId) -> Self {
        Self { file_set, root }
    }

    pub fn root(&self) -> FileId {
        self.root
    }

    pub fn file_for_path(&self, path: &FilePath) -> Option<FileId> {
        self.file_set.file_for_path(path)
    }

    pub fn path_for_file(&self, file_id: &FileId) -> Option<FilePath> {
        self.file_set.path_for_file(file_id)
    }
}

pub trait FileSystem {
    fn assign_or_get_file_id(&mut self, path: FilePath) -> FileId;

    fn path_for_file(&self, file_id: &FileId) -> Option<FilePath>;

    fn read_content(&self, file_id: &FileId) -> Option<String>;
}

pub fn collect_sources<FS: FileSystem>(
    db: &mut RootDatabase,
    fs: &mut FS,
    root_file: FileId,
) -> SourceRoot {
    let mut file_set = FileSet::new();

    let mut files = VecDeque::new();
    files.push_back(root_file);

    while let Some(file_id) = files.pop_front() {
        let parse = db.parse(file_id);

        let file_path = fs.path_for_file(&file_id).unwrap();
        let file_dir = file_path.parent().unwrap();
        file_set.insert(file_id, file_path);

        for include_path in list_include_paths(parse.syntax_node()) {
            let included_file_path = file_dir.join(include_path);
            let file_id = fs.assign_or_get_file_id(included_file_path.clone());
            let Some(file_content) = fs.read_content(&file_id) else {
                tracing::info!("failed to read file content: {:?}", included_file_path);
                continue;
            };

            files.push_back(file_id);
            db.set_file_content(file_id, Arc::from(file_content.as_str()));
        }
    }

    SourceRoot::new(file_set, root_file)
}

fn list_include_paths(root_node: SyntaxNode) -> Vec<String> {
    (|| -> Option<Vec<String>> {
        let source_file = ast::SourceFile::cast(root_node)?;
        let stmt_list = source_file.statement_list()?;
        let include_paths = stmt_list
            .statements()
            .filter_map(|stmt| match stmt {
                ast::Statement::Include(include) => include.path(),
                _ => None,
            })
            .map(|string| string.value())
            .collect();
        Some(include_paths)
    })()
    .unwrap_or_default()
}
