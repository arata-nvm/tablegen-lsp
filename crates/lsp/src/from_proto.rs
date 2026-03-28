use std::sync::Arc;

use async_lsp::lsp_types;
use ide::{
    analysis::Cancellable,
    file_system::{FileId, FilePosition, FileRange, FileSystem},
    line_index::LineIndex,
};
use text_size::{TextRange, TextSize};

use crate::{server::ServerSnapshot, vfs::url_to_file_path};

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
) -> Cancellable<Option<(FilePosition, Arc<LineIndex>)>> {
    let Ok(path) =
        url_to_file_path(&doc.text_document.uri).inspect_err(|e| tracing::warn!("file_pos: {e}"))
    else {
        return Ok(None);
    };
    let Some(file_id) = snap.vfs.file_for_path(&path) else {
        tracing::warn!("file not found in VFS: {path:?}");
        return Ok(None);
    };
    let line_index = snap.analysis.line_index(file_id)?;
    let pos = FilePosition::new(file_id, position(&line_index, doc.position));
    Ok(Some((pos, line_index)))
}

pub fn file_range(
    snap: &ServerSnapshot,
    doc: lsp_types::TextDocumentIdentifier,
    lsp_range: lsp_types::Range,
) -> Cancellable<Option<(FileRange, Arc<LineIndex>)>> {
    let Ok(path) = url_to_file_path(&doc.uri).inspect_err(|e| tracing::warn!("file_range: {e}"))
    else {
        return Ok(None);
    };
    let Some(file_id) = snap.vfs.file_for_path(&path) else {
        tracing::warn!("file not found in VFS: {path:?}");
        return Ok(None);
    };
    let line_index = snap.analysis.line_index(file_id)?;
    let range = FileRange::new(file_id, range(&line_index, lsp_range));
    Ok(Some((range, line_index)))
}

pub fn file(
    snap: &ServerSnapshot,
    doc: lsp_types::TextDocumentIdentifier,
) -> Cancellable<Option<(FileId, Arc<LineIndex>)>> {
    let Ok(path) = url_to_file_path(&doc.uri).inspect_err(|e| tracing::warn!("file: {e}")) else {
        return Ok(None);
    };
    let Some(file_id) = snap.vfs.file_for_path(&path) else {
        tracing::warn!("file not found in VFS: {path:?}");
        return Ok(None);
    };
    let line_index = snap.analysis.line_index(file_id)?;
    Ok(Some((file_id, line_index)))
}
