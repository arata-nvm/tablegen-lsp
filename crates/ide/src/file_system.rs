use std::collections::{HashMap, VecDeque};
use std::env;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;

use syntax::ast::AstNode;
use syntax::parser::{TextRange, TextSize};
use syntax::{ast, SyntaxNode, SyntaxNodePtr};

use crate::db::SourceDatabase;

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct FileId(pub u32);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct FilePosition {
    pub file: FileId,
    pub position: TextSize,
}

impl FilePosition {
    pub fn new(file: FileId, position: TextSize) -> Self {
        Self { file, position }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct FileRange {
    pub file: FileId,
    pub range: TextRange,
}

impl FileRange {
    pub fn new(file: FileId, range: TextRange) -> Self {
        Self { file, range }
    }
}

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

    pub fn contains(&self, file_id: &FileId) -> bool {
        self.id_to_path.contains_key(file_id)
    }

    pub fn file_for_path(&self, path: &FilePath) -> Option<FileId> {
        self.path_to_id.get(path).copied()
    }

    pub fn path_for_file(&self, file_id: &FileId) -> &FilePath {
        &self.id_to_path[file_id]
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

    pub fn path_for_file(&self, file_id: &FileId) -> &FilePath {
        self.file_set.path_for_file(file_id)
    }
}

pub trait FileSystem {
    fn assign_or_get_file_id(&mut self, path: FilePath) -> FileId;

    fn path_for_file(&self, file_id: &FileId) -> &FilePath;

    fn read_content(&self, file_path: &FilePath) -> Option<String>;
}

pub fn collect_sources<FS: FileSystem>(
    db: &mut dyn SourceDatabase,
    fs: &mut FS,
    root_file: FileId,
) -> SourceRoot {
    let mut file_set = FileSet::new();

    let mut files = VecDeque::new();
    files.push_back(root_file);
    while let Some(file_id) = files.pop_front() {
        let parse = db.parse(file_id);

        let file_path = fs.path_for_file(&file_id);
        let file_dir = file_path.parent().expect("file dir not found");
        file_set.insert(file_id, file_path.clone());

        let mut include_map = HashMap::new();
        // FIXME
        let include_dir_list = [
            file_dir,
            FilePath(
                PathBuf::from_str(
                    env::var("INCLUDE_DIR")
                        .expect("env `INCLUDE_DIR` is not set")
                        .as_str(),
                )
                .unwrap(),
            ),
        ];
        for (include_id, include_path) in list_includes(parse.syntax_node()) {
            if let Some(resolved_file_id) =
                resolve_include_file(db, fs, include_path, &include_dir_list)
            {
                include_map.insert(include_id, resolved_file_id);
                files.push_back(resolved_file_id);
            }
        }
        db.set_resolved_include_map(file_id, include_map)
    }

    SourceRoot::new(file_set, root_file)
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IncludeId(pub SyntaxNodePtr);

fn list_includes(root_node: SyntaxNode) -> Vec<(IncludeId, String)> {
    (|| -> Option<_> {
        let source_file = ast::SourceFile::cast(root_node)?;
        let stmt_list = source_file.statement_list()?;
        let include_paths = stmt_list
            .statements()
            .filter_map(|stmt| match stmt {
                ast::Statement::Include(include) => {
                    let id = IncludeId(SyntaxNodePtr::new(include.syntax()));
                    let path = include.path()?.value();
                    Some((id, path))
                }
                _ => None,
            })
            .collect();
        Some(include_paths)
    })()
    .unwrap_or_default()
}

fn resolve_include_file<FS: FileSystem>(
    db: &mut dyn SourceDatabase,
    fs: &mut FS,
    include_path: String,
    include_dir_list: &[FilePath],
) -> Option<FileId> {
    for include_dir in include_dir_list {
        let candidate_file_path = include_dir.join(&include_path);
        if let Some(file_content) = fs.read_content(&candidate_file_path) {
            let file_id = fs.assign_or_get_file_id(candidate_file_path.clone());
            db.set_file_content(file_id, Arc::from(file_content.as_str()));
            return Some(file_id);
        }
    }
    None
}
