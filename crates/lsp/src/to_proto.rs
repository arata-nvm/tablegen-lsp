use async_lsp::lsp_types;
use ide::db::SourceDatabase;
use text_size::{TextRange, TextSize};

use ide::handlers::diagnostics::{Diagnostic, FileRange};
use ide::handlers::document_symbol::DocumentSymbol;
use ide::line_index::LineIndex;

pub fn position(line_index: &LineIndex, position: TextSize) -> Option<lsp_types::Position> {
    let line = line_index.pos_to_line(position);
    let line_first = line_index.line_to_pos(line);
    let character = position - line_first;
    Some(lsp_types::Position::new(
        line.try_into().expect("line out of range"),
        character.into(),
    ))
}

pub fn range(line_index: &LineIndex, range: TextRange) -> Option<lsp_types::Range> {
    Some(lsp_types::Range::new(
        position(line_index, range.start())?,
        position(line_index, range.end())?,
    ))
}

pub fn file_range(db: &dyn SourceDatabase, file_range: FileRange) -> Option<lsp_types::Range> {
    let line_index = db.line_index(file_range.file);
    range(&line_index, file_range.range)
}

pub fn diagnostic(db: &dyn SourceDatabase, diag: Diagnostic) -> Option<lsp_types::Diagnostic> {
    Some(lsp_types::Diagnostic::new_simple(
        file_range(db, diag.range)?,
        diag.message,
    ))
}

#[allow(deprecated)]
pub fn document_symbol(
    line_index: &LineIndex,
    symbol: DocumentSymbol,
) -> Option<lsp_types::DocumentSymbol> {
    let range = range(line_index, symbol.range)?;
    Some(lsp_types::DocumentSymbol {
        name: symbol.name.to_string(),
        detail: None,
        kind: lsp_types::SymbolKind::CLASS,
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children: None,
    })
}