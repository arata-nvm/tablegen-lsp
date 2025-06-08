use std::fs;

use async_lsp::lsp_types::Url;

use ide::file_system::{FileId, FilePath, FileSet, FileSystem};

#[derive(Debug, Default)]
pub struct Vfs {
    file_set: FileSet,
    next_file_id: u32,
}

impl Vfs {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn file_for_path(&self, path: &FilePath) -> Option<FileId> {
        self.file_set.file_for_path(path)
    }

    fn alloc_file_id(&mut self) -> FileId {
        let file_id = FileId(self.next_file_id);
        self.next_file_id += 1;
        file_id
    }
}

impl FileSystem for Vfs {
    fn assign_or_get_file_id(&mut self, path: FilePath) -> FileId {
        match self.file_set.file_for_path(&path) {
            Some(file_id) => file_id,
            None => {
                let file_id = self.alloc_file_id();
                tracing::debug!("assign file id: {path:?} -> {file_id:?}",);
                self.file_set.insert(file_id, path);
                file_id
            }
        }
    }

    fn path_for_file(&self, file_id: &FileId) -> &FilePath {
        self.file_set.path_for_file(file_id)
    }

    fn read_content(&self, file_path: &FilePath) -> Option<String> {
        fs::read_to_string(&file_path.0).ok()
    }
}

pub(crate) trait UrlExt: Sized {
    fn to_file_path(&self) -> FilePath;
    fn from_file_path(path: &FilePath) -> Self;
}

impl UrlExt for Url {
    fn to_file_path(&self) -> FilePath {
        // TODO: Url::to_file_pathがErrを返したときの処理
        self.to_file_path()
            .expect("failed to convert url to file path")
            .as_path()
            .into()
    }

    fn from_file_path(path: &FilePath) -> Self {
        // TODO: Url::from_file_pathがErrを返したときの処理
        Url::from_file_path(&path.0).expect("failed to convert file path to url")
    }
}
