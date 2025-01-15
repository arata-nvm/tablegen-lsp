use std::{path::PathBuf, sync::Arc};

use ecow::EcoString;
use syntax::parser::{TextRange, TextSize};
use tblgen_alt::{
    error::{SourceLoc, SourceLocation},
    Record, RecordKeeper, SourceInfo, TableGenParser,
};

use crate::{
    db::SourceDatabase,
    file_system::{FileId, FilePath, FileRange, FileSystem},
    handlers::diagnostics::Diagnostic,
    symbol_map::{class::Class, def::Def, field::Field, template_arg::TemplateArgument, SymbolMap},
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
        let Ok(name) = name else {
            continue;
        };
        match convert_class(&mut symbol_map, fs, source_info, name, class) {
            Some(class) => {
                symbol_map.add_class(class);
            }
            None => continue,
        }
    }

    for (name, def) in record_keeper.defs() {
        let Ok(name) = name else {
            continue;
        };
        match convert_def(&mut symbol_map, fs, source_info, name, def) {
            Some(def) => {
                symbol_map.add_def(def);
            }
            None => continue,
        }
    }

    symbol_map
}

fn convert_class<FS: FileSystem>(
    symbol_map: &mut SymbolMap,
    fs: &mut FS,
    source_info: SourceInfo,
    name: &str,
    class: Record,
) -> Option<Class> {
    let name = EcoString::from(name);

    let source_loc = class.source_location();
    let define_loc = convert_source_location(fs, source_info, source_loc)?;

    let mut new_class = Class::new(name, define_loc);
    for value in class.values() {
        let Ok(value_name) = value.name.to_str() else {
            continue;
        };
        let name = EcoString::from(value_name);

        let source_loc = value.source_location();
        let define_loc = convert_source_location(fs, source_info, source_loc)?;

        if value.is_template_arg() {
            let template_arg = TemplateArgument::new(name, define_loc);
            new_class.add_template_arg(symbol_map, template_arg);
        } else {
            let field = Field::new(name, define_loc);
            new_class.add_field(symbol_map, field);
        }
    }

    Some(new_class)
}

fn convert_def<FS: FileSystem>(
    symbol_map: &mut SymbolMap,
    fs: &mut FS,
    source_info: SourceInfo,
    name: &str,
    class: Record,
) -> Option<Def> {
    let name = EcoString::from(name);

    let source_loc = class.source_location();
    let define_loc = convert_source_location(fs, source_info, source_loc)?;

    let mut new_def = Def::new(name, define_loc);
    for value in class.values() {
        let Ok(value_name) = value.name.to_str() else {
            continue;
        };
        let name = EcoString::from(value_name);

        let source_loc = value.source_location();
        let define_loc = convert_source_location(fs, source_info, source_loc)?;

        let field = Field::new(name, define_loc);
        new_def.add_field(symbol_map, field);
    }

    Some(new_def)
}

fn convert_source_location<FS: FileSystem>(
    fs: &mut FS,
    source_info: SourceInfo,
    source_loc: SourceLocation,
) -> Option<FileRange> {
    let file_pos = tblgen_alt::util::convert_loc(source_info, &source_loc)?;

    let filepath_ref = file_pos.filepath();
    let filepath = FilePath(PathBuf::from(filepath_ref.as_str().ok()?));
    let file_id = fs.assign_or_get_file_id(filepath);

    let pos: TextSize = file_pos.pos().into();
    Some(FileRange::new(file_id, TextRange::new(pos, pos)))
}

fn handle_tablegen_err(err: tblgen_alt::Error) -> Vec<Diagnostic> {
    tracing::debug!("{} {:?}", err, err);
    vec![]
}
