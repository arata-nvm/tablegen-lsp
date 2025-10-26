use std::sync::Arc;

use async_lsp::lsp_types;
use ide::{
    file_system::{FileId, FilePosition, FileRange, FileSystem},
    line_index::LineIndex,
};
use text_size::{TextRange, TextSize};

use crate::{server::ServerSnapshot, vfs::UrlExt};

pub fn position(line_index: &LineIndex, position: lsp_types::Position) -> TextSize {
    let pos_size = line_index.line_to_pos(position.line.try_into().unwrap());
    let char_size: TextSize = position.character.into();
    pos_size + char_size
}

pub fn range(line_index: &LineIndex, range: lsp_types::Range) -> TextRange {
    TextRange::new(
        position(line_index, range.start),
        position(line_index, range.end),
    )
}

pub fn file_pos(
    snap: &ServerSnapshot,
    doc: lsp_types::TextDocumentPositionParams,
) -> (FilePosition, Arc<LineIndex>) {
    let vfs = snap.vfs.read().unwrap();
    let path = UrlExt::to_file_path(&doc.text_document.uri);
    let file_id = vfs.file_for_path(&path).unwrap();
    let line_index = snap.analysis.line_index(file_id);
    let pos = FilePosition::new(file_id, position(&line_index, doc.position));
    (pos, line_index)
}

pub fn file_range(
    snap: &ServerSnapshot,
    doc: lsp_types::TextDocumentIdentifier,
    lsp_range: lsp_types::Range,
) -> (FileRange, Arc<LineIndex>) {
    let vfs = snap.vfs.read().unwrap();
    let path = UrlExt::to_file_path(&doc.uri);
    let file_id = vfs.file_for_path(&path).unwrap();
    let line_index = snap.analysis.line_index(file_id);
    let range = FileRange::new(file_id, range(&line_index, lsp_range));
    (range, line_index)
}

pub fn file(
    snap: &ServerSnapshot,
    doc: lsp_types::TextDocumentIdentifier,
) -> (FileId, Arc<LineIndex>) {
    let vfs = snap.vfs.read().unwrap();
    let path = UrlExt::to_file_path(&doc.uri);
    let file_id = vfs.file_for_path(&path).unwrap();
    let line_index = snap.analysis.line_index(file_id);
    (file_id, line_index)
}
