use syntax::error::SyntaxError;
use syntax::parser::TextRange;

use crate::{
    eval::{EvalDatabase, EvalError},
    file_system::FileId,
};

pub fn diagnostics(db: &dyn EvalDatabase, file_id: FileId) -> Vec<Diagnostic> {
    let mut diagnostic_list = Vec::new();

    let parse = db.parse(file_id);
    diagnostic_list.extend(parse.errors().iter().map(Diagnostic::from));

    let evaluation = db.eval(file_id);
    diagnostic_list.extend(evaluation.errors().iter().map(Diagnostic::from));

    diagnostic_list
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

impl From<&EvalError> for Diagnostic {
    fn from(value: &EvalError) -> Self {
        Self {
            range: value.range,
            message: value.message.to_string(),
        }
    }
}
