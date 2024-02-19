use async_lsp::lsp_types;

use ide::handlers::diagnostics::Diagnostic;
use ide::line_index::LineIndex;
use text_size::{TextRange, TextSize};

pub fn position(line_index: &LineIndex, position: TextSize) -> Option<lsp_types::Position> {
    let line = line_index.pos_to_line(position)?;
    let line_first = line_index.line_to_pos(line)?;
    let character = position - line_first;
    Some(lsp_types::Position::new(line as u32, character.into()))
}

pub fn range(line_index: &LineIndex, range: TextRange) -> Option<lsp_types::Range> {
    Some(lsp_types::Range::new(
        position(line_index, range.start())?,
        position(line_index, range.end())?,
    ))
}

pub fn diagnostic(line_index: &LineIndex, diag: Diagnostic) -> Option<lsp_types::Diagnostic> {
    Some(lsp_types::Diagnostic::new_simple(
        range(line_index, diag.range)?,
        diag.message,
    ))
}
