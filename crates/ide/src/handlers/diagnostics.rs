use std::collections::HashMap;

use syntax::parser::TextRange;

use crate::{eval::EvalDatabase, file_system::FileId};

pub fn diagnostics(db: &dyn EvalDatabase) -> HashMap<FileId, Vec<Diagnostic>> {
    let mut diagnostic_list = Vec::new();

    let source_root = db.source_root();
    let parse = db.parse(source_root.root());
    diagnostic_list.extend(parse.errors().iter().map(|err| {
        Diagnostic::new(
            FileRange::new(source_root.root(), err.range),
            err.message.to_string(),
        )
    }));

    let evaluation = db.eval();
    diagnostic_list.extend(evaluation.diagnostics().iter().cloned());

    let mut diagnostic_map = HashMap::new();
    for diagnostic in diagnostic_list {
        let file_id = diagnostic.range.file;
        let diagnostics = diagnostic_map.entry(file_id).or_insert_with(Vec::new);
        diagnostics.push(diagnostic);
    }
    diagnostic_map
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FileRange {
    pub file: FileId,
    pub range: TextRange,
}

impl FileRange {
    pub fn new(file: FileId, range: TextRange) -> Self {
        Self { file, range }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Diagnostic {
    pub range: FileRange,
    pub message: String,
}

impl Diagnostic {
    pub fn new(range: FileRange, message: impl Into<String>) -> Self {
        Self {
            range,
            message: message.into(),
        }
    }
}
