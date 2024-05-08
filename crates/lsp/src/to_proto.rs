use async_lsp::lsp_types;
use ide::file_system::FileRange;
use text_size::{TextRange, TextSize};

use ide::handlers::diagnostics::Diagnostic;
use ide::handlers::document_symbol::DocumentSymbol;
use ide::line_index::LineIndex;

pub fn position(line_index: &LineIndex, position: TextSize) -> lsp_types::Position {
    let line = line_index.pos_to_line(position);
    let line_first = line_index.line_to_pos(line);
    let character = position - line_first;
    lsp_types::Position::new(
        line.try_into().expect("line out of range"),
        character.into(),
    )
}

pub fn range(line_index: &LineIndex, range: TextRange) -> lsp_types::Range {
    lsp_types::Range::new(
        position(line_index, range.start()),
        position(line_index, range.end()),
    )
}

pub fn file_range(line_index: &LineIndex, file_range: FileRange) -> lsp_types::Range {
    range(&line_index, file_range.range)
}

pub fn diagnostic(line_index: &LineIndex, diag: Diagnostic) -> lsp_types::Diagnostic {
    lsp_types::Diagnostic::new_simple(file_range(line_index, diag.range), diag.message)
}

#[allow(deprecated)]
pub fn document_symbol(
    line_index: &LineIndex,
    symbol: DocumentSymbol,
) -> lsp_types::DocumentSymbol {
    let range = range(line_index, symbol.range);
    lsp_types::DocumentSymbol {
        name: symbol.name.to_string(),
        detail: None,
        kind: lsp_types::SymbolKind::CLASS,
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children: None,
    }
}
