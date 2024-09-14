use std::{path::PathBuf, str::Utf8Error, sync::Arc};

use ecow::EcoString;
use syntax::parser::{TextRange, TextSize};
use tblgen_alt::{error::SourceLoc, Record, RecordKeeper, SourceInfo, TableGenParser};

use crate::{
    db::SourceDatabase,
    file_system::{FileId, FilePath, FileRange, FileSystem},
    handlers::diagnostics::Diagnostic,
    symbol_map::{class::Class, SymbolMap},
};

#[salsa::query_group(EvalDatabaseStorage)]
pub trait EvalDatabase: SourceDatabase {
    #[salsa::input]
    fn symbol_map(&self, file_id: FileId) -> Arc<SymbolMap>;

    #[salsa::input]
    fn diagnostics(&self, file_id: FileId) -> Arc<Vec<Diagnostic>>;
}

pub fn eval<FS: FileSystem>(
    db: &dyn EvalDatabase,
    fs: &mut FS,
) -> Result<SymbolMap, Vec<Diagnostic>> {
    let source_root = db.source_root();
    let source_path = source_root.path_for_file(&source_root.root());
    let source_dir = source_path.parent().expect("Failed to get parent");

    let parser = TableGenParser::new()
        .add_source_file(&source_path.0.to_string_lossy())
        .expect("Failed to add source")
        .add_include_path(&source_dir.0.to_string_lossy());

    match parser.parse() {
        Ok(record_keeper) => Ok(convert_record_keeper(record_keeper, fs)),
        Err(err) => Err(handle_tablegen_err(err)),
    }
}

fn convert_record_keeper<FS: FileSystem>(record_keeper: RecordKeeper, fs: &mut FS) -> SymbolMap {
    let mut symbol_map = SymbolMap::default();

    let source_info = record_keeper.source_info();
    for (name, class) in record_keeper.classes() {
        match convert_class(source_info, fs, name, class) {
            Some(class) => {
                symbol_map.add_class(class);
            }
            None => continue,
        }
    }

    symbol_map
}

fn convert_class<FS: FileSystem>(
    source_info: SourceInfo,
    fs: &mut FS,
    name: Result<&str, Utf8Error>,
    class: Record,
) -> Option<Class> {
    let name = EcoString::from(name.ok()?);

    let source_loc = class.source_location();
    let file_pos = tblgen_alt::util::convert_loc(source_info, &source_loc)?;

    let filepath_ref = file_pos.filepath();
    let filepath = FilePath(PathBuf::from(filepath_ref.as_str().ok()?));
    let file_id = fs.assign_or_get_file_id(filepath);

    let pos: TextSize = file_pos.pos().into();
    let define_loc = FileRange::new(file_id, TextRange::new(pos, pos));

    Some(Class::new(name, define_loc))
}

fn handle_tablegen_err(err: tblgen_alt::Error) -> Vec<Diagnostic> {
    tracing::debug!("{} {:?}", err, err);
    vec![]
}
