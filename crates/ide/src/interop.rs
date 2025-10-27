use std::str::FromStr;
use std::{collections::HashMap, str::Utf8Error};

use ecow::EcoString;
pub use tblgen::diagnostic::DiagnosticKind;
use tblgen::error::SourceLoc;
use tblgen::{RecordKeeper, TableGenParser};

use crate::file_system::{FilePath, FilePosition, FileSystem};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TblgenParseResult {
    pub diagnostics: Vec<TblgenDiagnostic>,
    pub symbol_table: TblgenSymbolTable,
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
    fs: &impl FileSystem,
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
        symbol_table: TblgenSymbolTable::with_record_keeper(&result.record_keeper, fs),
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

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct TblgenSymbolTable {
    pub defs: HashMap<FilePosition, Vec<TblgenDef>>,
}

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct TblgenDef {
    pub name: EcoString,
    pub direct_super_classes: Vec<EcoString>,
}

impl TblgenSymbolTable {
    pub fn with_record_keeper(record_keeper: &RecordKeeper, fs: &impl FileSystem) -> Self {
        let mut defs = HashMap::new();
        for (name, record) in record_keeper.defs() {
            let (Ok(name), Some(tblgen_pos)) = (name, record.file_position(record_keeper)) else {
                continue;
            };

            let direct_super_classes = record
                .direct_super_classes()
                .filter_map(|class| class.name().ok())
                .map(EcoString::from)
                .collect::<Vec<_>>();
            let def = TblgenDef {
                name: EcoString::from(name),
                direct_super_classes,
            };

            let tblgen_filepath = tblgen_pos.filepath();
            let file_id = if let Ok(filepath) = tblgen_filepath.as_str()
                && let Ok(filepath) = FilePath::from_str(filepath)
                && let Some(file_id) = fs.file_for_path(&filepath)
            {
                file_id
            } else {
                continue;
            };

            let file_pos = FilePosition::new(file_id, tblgen_pos.pos().into());
            defs.entry(file_pos).or_insert_with(Vec::new).push(def);
        }

        TblgenSymbolTable { defs }
    }

    pub fn get_defs_at(&self, pos: &FilePosition) -> &[TblgenDef] {
        match self.defs.get(pos) {
            Some(defs) => defs,
            None => &[],
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{file_system::FileSystem, tests};

    #[test]
    fn def() {
        let (_, f) = tests::load_single_file("testdata/def.td");
        let root_file_id = f.root_file();
        let root_file_path = f.path_for_file(&root_file_id);
        let result =
            super::parse_source_unit_with_tblgen(root_file_path, &[], &f).expect("valid code");
        let mut defs = result.symbol_table.defs.into_iter().collect::<Vec<_>>();
        defs.sort_by_key(|(k, _)| k.position);
        insta::assert_debug_snapshot!(defs);
    }
}
