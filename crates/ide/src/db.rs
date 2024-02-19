use std::sync::Arc;

use syntax::Parse;

use crate::file::FileId;

#[salsa::database(SourceDatabaseStorage)]
#[derive(Default)]
pub struct RootDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for RootDatabase {}

#[salsa::query_group(SourceDatabaseStorage)]
pub trait SourceDatabase {
    #[salsa::input]
    fn file_content(&self, file_id: FileId) -> Arc<str>;

    fn parse(&self, file_id: FileId) -> Parse;
}

fn parse(db: &dyn SourceDatabase, file_id: FileId) -> Parse {
    let text = db.file_content(file_id);
    syntax::parse(&text)
}
