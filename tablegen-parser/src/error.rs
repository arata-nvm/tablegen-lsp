use ecow::EcoString;
use rowan::TextRange;
use std::fmt;

#[derive(Debug, Clone)]
pub struct SyntaxError {
    pub range: TextRange,
    pub message: EcoString,
}

impl SyntaxError {
    pub fn new(range: TextRange, message: impl Into<EcoString>) -> Self {
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
