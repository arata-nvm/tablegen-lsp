use async_lsp::lsp_types;
use ide::{
    file_system::{FileRange, FileSystem},
    handlers::completion::{CompletionItem, CompletionItemKind},
};
use text_size::{TextRange, TextSize};

use ide::handlers::diagnostics::Diagnostic;
use ide::line_index::LineIndex;

use crate::vfs::{UrlExt, Vfs};

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

pub fn location(vfs: &Vfs, line_index: &LineIndex, file_range: FileRange) -> lsp_types::Location {
    let path = vfs.path_for_file(&file_range.file);
    lsp_types::Location::new(
        UrlExt::from_file_path(path),
        range(line_index, file_range.range),
    )
}

pub fn diagnostic(line_index: &LineIndex, diag: Diagnostic) -> lsp_types::Diagnostic {
    lsp_types::Diagnostic::new_simple(range(line_index, diag.location.range), diag.message)
}

pub fn completion_item(item: CompletionItem) -> lsp_types::CompletionItem {
    let mut lsp_item = lsp_types::CompletionItem::new_simple(item.label, item.detail);
    lsp_item.kind = Some(match item.kind {
        CompletionItemKind::Keyword => lsp_types::CompletionItemKind::KEYWORD,
        CompletionItemKind::Type => lsp_types::CompletionItemKind::CLASS,
    });
    lsp_item
}
