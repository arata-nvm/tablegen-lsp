use std::collections::HashMap;

use crate::{
    file_system::{FileId, FileRange},
    index::IndexDatabase,
};

pub fn exec(db: &dyn IndexDatabase) -> HashMap<FileId, Vec<Diagnostic>> {
    let mut diagnostic_list = Vec::new();

    let source_root = db.source_root();
    let parse = db.parse(source_root.root());
    diagnostic_list.extend(parse.errors().iter().map(|err| {
        Diagnostic::new(
            FileRange::new(source_root.root(), err.range),
            err.message.to_string(),
        )
    }));

    let index = db.index();
    diagnostic_list.extend(index.diagnostics().iter().cloned());

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

    use super::Diagnostic;

    fn check(s: &str) -> Vec<Diagnostic> {
        let (db, f) = tests::single_file(s);
        super::exec(&db).remove(&f.root_file()).unwrap()
    }

    #[test]
    fn syntax() {
        insta::assert_debug_snapshot!(check("clas"));
    }

    #[test]
    fn eval() {
        insta::assert_debug_snapshot!(check(r#"include "not_exist.td""#));
    }

    #[test]
    fn update_diag() {
        let (mut db, f) = tests::single_file("class Foo");
        let diags1 = super::exec(&db);

        db.set_file_content(f.root_file(), Arc::from("class Foo;"));
        let diags2 = super::exec(&db);

        insta::assert_debug_snapshot!((diags1, diags2));
    }

    #[test]
    fn class_already_defined() {
        insta::assert_debug_snapshot!(check("class Foo; class Foo { int b; }"));
        insta::assert_debug_snapshot!(check("class Foo { int a; } class Foo { int b; }"));
    }
}
