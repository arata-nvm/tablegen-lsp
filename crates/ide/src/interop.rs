use std::{collections::HashSet, str::Utf8Error};

use ecow::EcoString;
use tblgen::TableGenParser;
pub use tblgen::diagnostic::DiagnosticKind;

use crate::file_system::FilePath;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TblgenParseResult {
    pub diagnostics: Vec<TblgenDiagnostic>,
    pub symbol_table: TblgenSymbolTable,
}

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct TblgenSymbolTable {
    pub def_names: HashSet<EcoString>,
}

impl TblgenSymbolTable {
    pub fn new(def_names: HashSet<EcoString>) -> Self {
        TblgenSymbolTable { def_names }
    }

    pub fn has_def(&self, name: &str) -> bool {
        self.def_names.contains(name)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TblgenDiagnostic {
    pub kind: tblgen::diagnostic::DiagnosticKind,
    pub message: String,
    pub filename: String,
    pub line: i32,
    pub column: i32,
}

pub fn parse_source_unit_with_tblgen(
    root_file: &FilePath,
    include_dirs: &[FilePath],
) -> Result<TblgenParseResult, Utf8Error> {
    let mut parser = TableGenParser::new().add_source_file(root_file.to_str());
    if let Some(parent) = root_file.parent() {
        parser = parser.add_include_directory(parent.to_str());
    }
    for include_dir in include_dirs {
        parser = parser.add_include_directory(include_dir.to_str());
    }
    let result = parser.parse();

    Ok(TblgenParseResult {
        diagnostics: result
            .diagnostics
            .into_iter()
            .map(convert_diagnostic)
            .collect::<Result<_, _>>()?,
        symbol_table: TblgenSymbolTable::new(
            result
                .record_keeper
                .defs()
                .filter_map(|(name, _)| name.ok())
                .map(EcoString::from)
                .collect(),
        ),
    })
}

fn convert_diagnostic(
    diag: tblgen::diagnostic::Diagnostic<'_>,
) -> Result<TblgenDiagnostic, Utf8Error> {
    let message = diag.message();
    let filename = diag.filename();
    Ok(TblgenDiagnostic {
        kind: diag.kind(),
        message: message.as_str()?.to_string(),
        filename: filename.as_str()?.to_string(),
        line: diag.line() - 1,
        column: diag.column(),
    })
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::file_system::FilePath;

    #[test]
    fn foreach() {
        let root_file = FilePath::from(Path::new("testdata/foreach.td"));
        let result = super::parse_source_unit_with_tblgen(&root_file, &[]).expect("valid code");
        assert!(result.diagnostics.is_empty());
        assert!(!result.symbol_table.has_def("Foo"));
        assert!(result.symbol_table.has_def("foo1"));
        assert!(result.symbol_table.has_def("foo2"));
        assert!(!result.symbol_table.has_def("foo"));
    }
}
