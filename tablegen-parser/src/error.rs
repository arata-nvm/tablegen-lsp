use crate::parser::Range;
use ecow::EcoString;
use std::fmt;

#[derive(Debug, Clone)]
pub struct SyntaxError {
    pub range: Range,
    pub message: EcoString,
}

impl SyntaxError {
    pub fn new(range: Range, message: impl Into<EcoString>) -> Self {
        Self {
            range,
            message: message.into(),
        }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.range.start, self.range.end, self.message
        )
    }
}
