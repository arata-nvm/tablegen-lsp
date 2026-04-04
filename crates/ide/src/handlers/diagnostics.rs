use crate::{
    db::IndexDatabase,
    file_system::{FileRange, SourceUnitId},
    index::Index,
    interop::TblgenDiagnostic,
};

pub fn exec(db: &dyn IndexDatabase, source_unit_id: SourceUnitId) -> Vec<Diagnostic> {
    let mut diagnotics = Vec::new();

    let source_unit = db.source_unit(source_unit_id).source_unit(db);
    for file_id in source_unit.iter_files() {
        let parse = db.parse(file_id);
        diagnotics.extend(parse.errors().iter().map(|err| {
            Diagnostic::new_lsp(FileRange::new(file_id, err.range), err.message.to_string())
        }));
    }

    let Index { diagnostics, .. } = &*db.index(source_unit_id);
    diagnotics.extend(diagnostics.iter().cloned());

    if let Some(result) = db.tblgen_result(source_unit_id) {
        diagnotics.extend(
            result
                .diagnostics(db)
                .iter()
                .cloned()
                .map(Diagnostic::Tblgen),
        );
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

    pub fn is_include_error(&self) -> bool {
        match self {
            Self::Lsp(diag) => diag.message.starts_with("include file not found:"),
            Self::Tblgen(diag) => diag.message.contains("could not find include file"),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;
    use std::sync::Arc;

    use crate::{db::Database, tests};

    use super::Diagnostic;

    fn check(s: &str) -> String {
        let (db, f) = tests::single_file(s);
        let diagnostics = super::exec(&db, f.source_unit_id());
        render_diagnostics(&diagnostics, &f)
    }

    fn render_diagnostics(diags: &[Diagnostic], f: &tests::Fixture) -> String {
        let mut out = String::new();
        for diag in diags {
            match diag {
                Diagnostic::Lsp(d) => {
                    out.push_str(&tests::render_file_range_block(
                        f,
                        d.location,
                        [d.message.as_str()],
                    ));
                }
                Diagnostic::Tblgen(d) => {
                    let _ = writeln!(out, "tblgen:{}:{} {}", d.line, d.column, d.message);
                }
            }
            out.push('\n');
        }
        out
    }

    #[test]
    fn syntax() {
        insta::assert_snapshot!(check("clas"));
    }

    #[test]
    fn eval() {
        insta::assert_snapshot!(check(r#"include "not_exist.td""#));
    }

    #[test]
    fn is_include_error() {
        let (db, f) = tests::single_file(r#"include "not_exist.td""#);
        let diagnostics = super::exec(&db, f.source_unit_id());
        assert!(diagnostics.iter().any(Diagnostic::is_include_error));
    }

    #[test]
    fn is_not_include_error() {
        let (db, f) = tests::single_file("class Foo : Bar;");
        let diagnostics = super::exec(&db, f.source_unit_id());
        assert!(!diagnostics.iter().any(Diagnostic::is_include_error));
    }

    #[test]
    fn update_diag() {
        let (mut db, f) = tests::single_file("class Foo");
        let diags1 = super::exec(&db, f.source_unit_id());

        db.set_file(f.root_file(), Arc::from("class Foo;"));
        let diags2 = super::exec(&db, f.source_unit_id());

        let before = render_diagnostics(&diags1, &f);
        let (_, f_after) = tests::single_file("class Foo;");
        let after = render_diagnostics(&diags2, &f_after);
        insta::assert_snapshot!(format!("before:\n{before}\nafter:\n{after}"));
    }

    #[test]
    fn class_already_defined() {
        insta::assert_snapshot!(check("class Foo; class Foo { int b; }"));
        insta::assert_snapshot!(check("class Foo { int a; } class Foo { int b; }"));
    }

    #[test]
    fn class_cyclic_inheritance() {
        insta::assert_snapshot!(check("class Foo : Foo;"));
    }

    #[test]
    fn strconcat() {
        insta::assert_snapshot!(check(
            "class Foo<string prefix> { string Name = prefix # _POSTFIX; } "
        ));
        insta::assert_snapshot!(check(
            "class Foo<string suffix> { string Name = PREFIX_ # suffix; } "
        ));
    }

    #[test]
    fn typeof_foreach_init() {
        insta::assert_snapshot!(check(
            r#"class Foo<int i>; foreach i = 1...3 in def : Foo<i>;"#
        ));
        insta::assert_snapshot!(check(r#"foreach i = "hoge"...3 in def foo#i;"#));
        insta::assert_snapshot!(check(r#"foreach i = 1..."fuga" in def foo#i;"#));
        insta::assert_snapshot!(check(r#"foreach i = "hoge"..."fuga" in def foo#i;"#));

        insta::assert_snapshot!(check(
            r#"class Foo<int i>; foreach i = [1, 2] in def : Foo<i>;"#
        ));
        insta::assert_snapshot!(check(r#"foreach i = "hoge" in def foo#i;"#));

        insta::assert_snapshot!(check(
            r#"class Foo<string s>; foreach s = ["a", "b"] in def : Foo<s>;"#
        ));
    }
}
