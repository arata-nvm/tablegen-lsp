use std::{collections::HashMap, fmt::Write, path::Path, sync::Arc};

use syntax::parser::TextRange;

use crate::{
    db::{Database, RootDatabase},
    file_system::{
        self, FileId, FilePath, FilePosition, FileRange, FileSet, FileSystem, SourceUnitId,
    },
    interop,
};

const DEFAULT_FILE_PATH: &str = "/main.td";
const MARKER_INDICATOR: char = '$';

pub fn single_file(fixture: &str) -> (RootDatabase, Fixture) {
    let mut f = Fixture::single_file(fixture);
    let db = TestDatabase::new(&mut f);
    (db, f)
}

pub fn multiple_files(fixture: &str) -> (RootDatabase, Fixture) {
    let mut f = Fixture::multiple_files(fixture);
    let db = TestDatabase::new(&mut f);
    (db, f)
}

pub fn load_single_file(path: &str) -> (RootDatabase, Fixture) {
    let mut f = Fixture::load_single_file(path);
    let db = TestDatabase::new(&mut f);
    (db, f)
}

pub fn load_single_file_with_tblgen(path: &str) -> (RootDatabase, Fixture) {
    let mut f = Fixture::load_single_file(path);
    let db = TestDatabase::new_with_tblgen(&mut f);
    (db, f)
}

pub fn render_file_range_block<S, I>(
    fixture: &Fixture,
    file_range: FileRange,
    prefix_lines: I,
) -> String
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let path = fixture.path_for_file(&file_range.file);
    let content = fixture.file_content(&file_range.file);
    let (line, marker) = render_line_marker(&content, file_range.range);

    let mut out = String::new();
    let _ = writeln!(out, "{}", path.to_str());
    for line_prefix in prefix_lines {
        let _ = writeln!(out, "{}", line_prefix.as_ref());
    }
    let _ = writeln!(out, "{line}");
    let _ = writeln!(out, "{marker}");
    out
}

fn render_line_marker(content: &str, range: TextRange) -> (String, String) {
    let start = usize::try_from(u32::from(range.start())).unwrap();
    let end = usize::try_from(u32::from(range.end())).unwrap();
    assert!(start <= content.len());
    assert!(end <= content.len());

    let line_start = content[..start].rfind('\n').map_or(0, |i| i + 1);
    let line_end = content[end..].find('\n').map_or(content.len(), |i| end + i);

    let line = content[line_start..line_end].to_string();
    let start_col = content[line_start..start].chars().count();
    let end_col = content[line_start..end].chars().count();
    let marker = format!(
        "{}{}",
        " ".repeat(start_col),
        "^".repeat((end_col - start_col).max(1))
    );
    (line, marker)
}

pub fn render_inline_ranges(content: &str, ranges: impl IntoIterator<Item = TextRange>) -> String {
    const START_MARKER: &str = "{|";
    const END_MARKER: &str = "|}";

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    enum EventKind {
        End,
        Start,
    }

    let mut events = Vec::new();
    for range in ranges {
        let start = usize::try_from(u32::from(range.start())).unwrap();
        let end = usize::try_from(u32::from(range.end())).unwrap();
        assert!(start <= end);
        assert!(start <= content.len());
        assert!(end <= content.len());

        events.push((start, EventKind::Start));
        events.push((end, EventKind::End));
    }
    events.sort_by_key(|&(pos, kind)| (pos, kind));

    let mut out = String::new();
    let mut cursor = 0;
    for (pos, kind) in events {
        if cursor < pos {
            out.push_str(&content[cursor..pos]);
        }
        out.push_str(match kind {
            EventKind::Start => START_MARKER,
            EventKind::End => END_MARKER,
        });
        cursor = pos;
    }
    out.push_str(&content[cursor..]);
    out
}

pub struct TestDatabase {}

impl TestDatabase {
    #[allow(clippy::new_ret_no_self)]
    fn new(f: &mut Fixture) -> RootDatabase {
        let mut db = RootDatabase::default();
        for (file_id, content) in f.files() {
            db.set_file(file_id, Arc::from(content));
        }

        let id = SourceUnitId::from_root_file(f.root_file());
        let source_unit = file_system::collect_sources(&mut db, f, f.root_file(), &[]);
        db.set_source_unit(id, Arc::new(source_unit));
        db
    }

    fn new_with_tblgen(f: &mut Fixture) -> RootDatabase {
        let mut db = Self::new(f);
        let root_file_id = f.root_file();
        let root_file_path = f.path_for_file(&root_file_id);

        let id = SourceUnitId::from_root_file(root_file_id);
        let result = interop::parse_source_unit_with_tblgen(&root_file_path, &[], f)
            .expect("failed to parse source unit with tblgen");
        db.set_tblgen_result(id, result);
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
