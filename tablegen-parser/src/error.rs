use std::fmt;

use ecow::EcoString;

pub type Span = std::ops::Range<usize>;

#[derive(Debug)]
pub struct SyntaxError {
    pub span: Span,
    pub message: EcoString,
}

impl SyntaxError {
    pub fn new(span: Span, message: impl Into<EcoString>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.span.start, self.span.end, self.message)
    }
}
