use std::{collections::HashMap, path::Path};

use async_lsp::lsp_types;
use ide::{
    file_system::{FileId, FilePath, FileSystem, SourceUnit},
    handlers::diagnostics::Diagnostic,
};

use crate::{server::ServerSnapshot, to_proto};

#[derive(Debug, Default)]
pub struct DiagnosticCollection {
    diagnostics: HashMap<FileId, Vec<lsp_types::Diagnostic>>,
}

impl DiagnosticCollection {
    pub fn add_source_unit_files(&mut self, source_unit: &SourceUnit) {
        for file_id in source_unit.iter_files() {
            self.diagnostics.entry(file_id).or_default();
        }
    }

    pub fn push(&mut self, snap: &ServerSnapshot, diag: Diagnostic) {
        match diag {
            ide::handlers::diagnostics::Diagnostic::Lsp(diag) => {
                let file_id = diag.location.file;
                let line_index = snap.analysis.line_index(file_id);

                let lsp_diag = to_proto::diagnostic(&line_index, diag);
                self.diagnostics.entry(file_id).or_default().push(lsp_diag);
            }
            ide::handlers::diagnostics::Diagnostic::Tblgen(diag) => {
                let file_path = FilePath::from(Path::new(&diag.filename));
                let mut vfs = snap.vfs.clone();
                let file_id = vfs.assign_or_get_file_id(file_path);

                let lsp_diag = to_proto::tblgen_diagnostic(diag);
                self.diagnostics.entry(file_id).or_default().push(lsp_diag);
            }
        }
    }

    pub fn extend(&mut self, snap: &ServerSnapshot, diags: Vec<Diagnostic>) {
        for diag in diags {
            self.push(snap, diag);
        }
    }
}

impl IntoIterator for DiagnosticCollection {
    type Item = (FileId, Vec<lsp_types::Diagnostic>);
    type IntoIter = std::collections::hash_map::IntoIter<FileId, Vec<lsp_types::Diagnostic>>;

    fn into_iter(self) -> Self::IntoIter {
        self.diagnostics.into_iter()
    }
}
