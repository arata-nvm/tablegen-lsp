use ecow::EcoString;
use rowan::TextRange;
use std::fmt;

#[derive(Debug, Clone)]
pub struct TableGenError {
    pub range: TextRange,
    pub message: EcoString,
}

impl TableGenError {
    pub fn new(range: TextRange, message: impl Into<EcoString>) -> Self {
        Self {
            range,
            message: message.into(),
        }
    }
}

impl fmt::Display for TableGenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}", self.range, self.message)
    }
}
