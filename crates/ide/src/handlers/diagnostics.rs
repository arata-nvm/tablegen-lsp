use syntax::error::SyntaxError;
use syntax::parser::TextRange;

use crate::db::SourceDatabase;
use crate::file_system::FileId;

pub fn diagnostics(db: &dyn SourceDatabase, file_id: FileId) -> Vec<Diagnostic> {
    let parse = db.parse(file_id);
    parse.errors().iter().map(Diagnostic::from).collect()
}

pub struct Diagnostic {
    pub range: TextRange,
    pub message: String,
}

impl Diagnostic {
    pub fn new(range: TextRange, message: String) -> Self {
        Self { range, message }
    }
}

impl From<&SyntaxError> for Diagnostic {
    fn from(value: &SyntaxError) -> Self {
        Self {
            range: value.range,
            message: value.message.to_string(),
        }
    }
}
