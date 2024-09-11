use std::sync::Arc;

use tblgen_alt::{RecordKeeper, TableGenParser};

use crate::{db::SourceDatabase, handlers::diagnostics::Diagnostic, symbol_map::SymbolMap};

#[salsa::query_group(EvalDatabaseStorage)]
pub trait EvalDatabase: SourceDatabase {
    fn eval(&self) -> Arc<Evaluation>;
}

#[derive(Debug, Eq, PartialEq)]
pub struct Evaluation {
    pub symbol_map: SymbolMap,
    pub diagnostics: Vec<Diagnostic>,
}

fn eval(db: &dyn EvalDatabase) -> Arc<Evaluation> {
    let source_root = db.source_root();
    let source = db.file_content(source_root.root());

    let parser = TableGenParser::new()
        .add_source(&source)
        .expect("Failed to add source");

    match parser.parse() {
        Ok(record_keeper) => convert_record_keeper(record_keeper),
        Err(err) => handle_tablegen_err(err),
    }

    Arc::new(Evaluation {
        symbol_map: SymbolMap::default(),
        diagnostics: vec![],
    })
}

fn convert_record_keeper(record_keeper: RecordKeeper) {}

fn handle_tablegen_err(err: tblgen_alt::Error) {}
