use std::fmt;

use rowan::TextRange;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SyntaxError {
    pub range: TextRange,
    pub message: String,
}

impl SyntaxError {
    pub fn new(range: TextRange, message: impl Into<String>) -> Self {
        Self {
            range,
            message: message.into(),
        }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}", self.range, self.message)
    }
}
