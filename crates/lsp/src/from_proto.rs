use std::sync::Arc;

use async_lsp::lsp_types;
use ide::{file_system::FileId, line_index::LineIndex};

use crate::{server::ServerSnapshot, vfs::UrlExt};

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
