use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct FileId(pub u32);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FilePath(pub PathBuf);

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
