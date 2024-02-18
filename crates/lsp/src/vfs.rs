use async_lsp::lsp_types::Url;

use ide::file::{FileId, FilePath, FileSet};

#[derive(Debug, Default)]
pub struct Vfs {
    file_set: FileSet,
    next_file_id: u32,
}

impl Vfs {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn assign_or_get_file_id(&mut self, path: FilePath) -> FileId {
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

    fn alloc_file_id(&mut self) -> FileId {
        let file_id = FileId(self.next_file_id);
        self.next_file_id += 1;
        file_id
    }
}

pub(crate) trait UrlExt: Sized {
    fn to_file_path(&self) -> FilePath;
    fn from_file_path(path: &FilePath) -> Self;
}

impl UrlExt for Url {
    fn to_file_path(&self) -> FilePath {
        // TODO: Url::to_file_pathがErrを返したときの処理
        self.to_file_path().unwrap().as_path().into()
    }

    fn from_file_path(path: &FilePath) -> Self {
        // TODO: Url::from_file_pathがErrを返したときの処理
        Url::from_file_path(&path.0).unwrap()
    }
}
