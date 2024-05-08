use syntax::parser::TextRange;

use crate::{
    eval::{EvalDatabase, EvalError},
    file_system::FileId,
};

pub fn diagnostics(db: &dyn EvalDatabase) -> Vec<Diagnostic> {
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
    diagnostic_list.extend(evaluation.errors().iter().cloned().map(Diagnostic::from));

    diagnostic_list
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

pub struct Diagnostic {
    pub range: FileRange,
    pub message: String,
}

impl Diagnostic {
    pub fn new(range: FileRange, message: String) -> Self {
        Self { range, message }
    }
}

impl From<EvalError> for Diagnostic {
    fn from(value: EvalError) -> Self {
        Self {
            range: value.range,
            message: value.message.to_string(),
        }
    }
}
