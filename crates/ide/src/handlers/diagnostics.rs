use crate::{
    file_system::{FileRange, SourceUnitId},
    index::IndexDatabase,
    interop::TblgenDiagnostic,
};

pub fn exec(db: &dyn IndexDatabase, source_unit_id: SourceUnitId) -> Vec<Diagnostic> {
    let mut diagnotics = Vec::new();

    let source_unit = db.source_unit(source_unit_id);
    let parse = db.parse(source_unit.root());
    diagnotics.extend(parse.errors().iter().map(|err| {
        Diagnostic::new_lsp(
            FileRange::new(source_unit.root(), err.range),
            err.message.to_string(),
        )
    }));

    let index = db.index(source_unit_id);
    diagnotics.extend(index.diagnostics().iter().cloned());

    if let Some(tblgen_diags) = db.tblgen_diagnostics(source_unit_id) {
        diagnotics.extend(tblgen_diags.iter().cloned().map(Diagnostic::Tblgen));
    }

    diagnotics
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Diagnostic {
    Lsp(LspDiagnostic),
    Tblgen(TblgenDiagnostic),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LspDiagnostic {
    pub location: FileRange,
    pub message: String,
}

impl Diagnostic {
    pub fn new_lsp(location: FileRange, message: impl Into<String>) -> Self {
        Self::Lsp(LspDiagnostic {
            location,
            message: message.into(),
        })
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::{db::SourceDatabase, tests};

    use super::Diagnostic;

    fn check(s: &str) -> Vec<Diagnostic> {
        let (db, f) = tests::single_file(s);
        super::exec(&db, f.source_unit_id())
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
        let diags1 = super::exec(&db, f.source_unit_id());

        db.set_file_content(f.root_file(), Arc::from("class Foo;"));
        let diags2 = super::exec(&db, f.source_unit_id());

        insta::assert_debug_snapshot!((diags1, diags2));
    }

    #[test]
    fn class_already_defined() {
        insta::assert_debug_snapshot!(check("class Foo; class Foo { int b; }"));
        insta::assert_debug_snapshot!(check("class Foo { int a; } class Foo { int b; }"));
    }

    #[test]
    fn class_cyclic_inheritance() {
        insta::assert_debug_snapshot!(check("class Foo : Foo;"));
    }
}
