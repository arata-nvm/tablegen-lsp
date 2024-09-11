use std::collections::HashMap;

use crate::{
    db::SourceDatabase,
    file_system::{FileId, FileRange},
};

pub fn exec(db: &dyn SourceDatabase) -> HashMap<FileId, Vec<Diagnostic>> {
    let mut diagnostic_list = Vec::new();

    let source_root = db.source_root();
    let parse = db.parse(source_root.root());
    diagnostic_list.extend(parse.errors().iter().map(|err| {
        Diagnostic::new(
            FileRange::new(source_root.root(), err.range),
            err.message.to_string(),
        )
    }));

    let mut diagnostic_map = HashMap::new();
    for file_id in db.source_root().iter_files() {
        diagnostic_map.insert(file_id, Vec::new());
    }

    for diagnostic in diagnostic_list {
        let file_id = diagnostic.location.file;
        let diagnostics = diagnostic_map.entry(file_id).or_insert_with(Vec::new);
        diagnostics.push(diagnostic);
    }
    diagnostic_map
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Diagnostic {
    pub location: FileRange,
    pub message: String,
}

impl Diagnostic {
    pub fn new(location: FileRange, message: impl Into<String>) -> Self {
        Self {
            location,
            message: message.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::{db::SourceDatabase, tests};

    #[test]
    fn syntax() {
        let (db, _) = tests::single_file("clas");
        let diags = super::exec(&db);
        insta::assert_debug_snapshot!(diags);
    }

    #[test]
    fn eval() {
        let (db, _) = tests::single_file(r#"include "not_exist.td""#);
        let diags = super::exec(&db);
        insta::assert_debug_snapshot!(diags);
    }

    #[test]
    fn update_diag() {
        let (mut db, f) = tests::single_file("class Foo");
        let diags1 = super::exec(&db);

        db.set_file_content(f.root_file(), Arc::from("class Foo;"));
        let diags2 = super::exec(&db);

        insta::assert_debug_snapshot!((diags1, diags2));
    }
}
