use std::{
    fs,
    sync::{Arc, RwLock},
};

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
                tracing::info!("assign file id: {path:?} -> {file_id:?}",);
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
        fs::read_to_string(&file_path.0).ok()
    }
}

pub struct SharedFs<FS: FileSystem> {
    fs: Arc<RwLock<FS>>,
}

impl<FS: FileSystem> SharedFs<FS> {
    pub fn new(fs: FS) -> Self {
        Self {
            fs: Arc::new(RwLock::new(fs)),
        }
    }
}

impl<FS: FileSystem> Clone for SharedFs<FS> {
    fn clone(&self) -> Self {
        Self {
            fs: Arc::clone(&self.fs),
        }
    }
}

impl<FS: FileSystem> FileSystem for SharedFs<FS> {
    fn assign_or_get_file_id(&mut self, path: FilePath) -> FileId {
        self.fs.write().unwrap().assign_or_get_file_id(path)
    }

    fn file_for_path(&self, path: &FilePath) -> Option<FileId> {
        self.fs.read().unwrap().file_for_path(path)
    }

    fn path_for_file(&self, file_id: &FileId) -> FilePath {
        self.fs.read().unwrap().path_for_file(file_id)
    }

    fn read_content(&self, file_path: &FilePath) -> Option<String> {
        self.fs.read().unwrap().read_content(file_path)
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
            .into()
    }

    fn from_file_path(path: &FilePath) -> Self {
        // TODO: Url::from_file_pathがErrを返したときの処理
        Url::from_file_path(&path.0).expect("failed to convert file path to url")
    }
}
