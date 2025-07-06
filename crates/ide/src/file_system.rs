use std::collections::{HashMap, VecDeque};
use std::path::{Path, PathBuf};

use ecow::EcoString;
use syntax::ast::AstNode;
use syntax::parser::{TextRange, TextSize};
use syntax::{SyntaxNode, SyntaxNodePtr, ast};

use crate::db::{Db, parse};

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

    pub fn iter_files(&self) -> impl Iterator<Item = FileId> + '_ {
        self.id_to_path.keys().copied()
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct SourceUnitId(FileId);

impl SourceUnitId {
    pub fn from_root_file(root_file: FileId) -> Self {
        Self(root_file)
    }
}

#[derive(Debug)]
pub struct SourceUnit {
    root: FileId,
    includes: HashMap<FileId, IncludeMap>,
}

impl SourceUnit {
    pub fn root(&self) -> FileId {
        self.root
    }

    pub fn iter_files(&self) -> impl Iterator<Item = FileId> + '_ {
        self.includes.keys().copied()
    }

    pub fn include_map(&self, file_id: &FileId) -> Option<&IncludeMap> {
        self.includes.get(file_id)
    }
}

pub trait FileSystem {
    fn assign_or_get_file_id(&mut self, path: FilePath) -> FileId;

    fn path_for_file(&self, file_id: &FileId) -> &FilePath;

    fn read_content(&self, file_path: &FilePath) -> Option<String>;
}

pub fn collect_sources<FS: FileSystem>(
    db: &mut dyn Db,
    fs: &mut FS,
    root: FileId,
    include_dirs: &[FilePath],
) -> SourceUnit {
    let mut includes = HashMap::new();

    let mut queue = VecDeque::new();
    queue.push_back(root);
    while let Some(file_id) = queue.pop_front() {
        if includes.contains_key(&file_id) {
            continue;
        }

        let parse = parse(db, file_id);

        let file_path = fs.path_for_file(&file_id);
        let file_dir = file_path.parent().expect("file dir not found");

        let mut include_dir_list = vec![file_dir];
        include_dir_list.extend(include_dirs.iter().cloned());

        let mut include_map = HashMap::new();
        for (include_id, include_path) in list_includes(parse.syntax_node()) {
            let Some(resolved_file_id) =
                resolve_include_file(db, fs, include_path, &include_dir_list)
            else {
                continue;
            };

            include_map.insert(include_id, resolved_file_id);
            queue.push_back(resolved_file_id);
        }
        includes.insert(file_id, include_map);
    }

    SourceUnit { root, includes }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IncludeId(pub SyntaxNodePtr);

pub type IncludeMap = HashMap<IncludeId, FileId>;

fn list_includes(root_node: SyntaxNode) -> Vec<(IncludeId, EcoString)> {
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
    db: &mut dyn Db,
    fs: &mut FS,
    include_path: EcoString,
    include_dir_list: &[FilePath],
) -> Option<FileId> {
    for include_dir in include_dir_list {
        let candidate_file_path = include_dir.join(include_path.as_str());
        if let Some(file_content) = fs.read_content(&candidate_file_path) {
            let file_id = fs.assign_or_get_file_id(candidate_file_path.clone());
            db.set_file_content(file_id, file_content.as_str());
            return Some(file_id);
        }
    }
    None
}
