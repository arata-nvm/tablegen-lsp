use std::str::Utf8Error;

use tblgen::TableGenParser;
pub use tblgen::diagnostic::DiagnosticKind;

use crate::file_system::FilePath;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TblgenParseResult {
    pub diagnostics: Vec<TblgenDiagnostic>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TblgenDiagnostic {
    pub kind: tblgen::diagnostic::DiagnosticKind,
    pub message: String,
    pub filename: String,
    pub column: i32,
    pub line: i32,
}

pub fn parse_source_unit_with_tblgen(
    root_file: &FilePath,
    include_dirs: &[FilePath],
) -> Result<TblgenParseResult, Utf8Error> {
    let mut parser = TableGenParser::new().add_source_file(root_file.to_str());
    for include_dir in include_dirs {
        parser = parser.add_include_directory(include_dir.to_str());
    }
    let result = parser.parse();
    let diagnostics = result
        .diagnostics
        .into_iter()
        .map(convert_diagnostic)
        .collect::<Result<_, _>>()?;
    Ok(TblgenParseResult { diagnostics })
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
        column: diag.column(),
        line: diag.line(),
    })
}
