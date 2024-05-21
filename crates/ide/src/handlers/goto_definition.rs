use crate::eval::EvalDatabase;
use crate::file_system::{FilePosition, FileRange};

pub fn exec(db: &dyn EvalDatabase, pos: FilePosition) -> Option<FileRange> {
    let evaluation = db.eval();
    let symbol_map = evaluation.symbol_map();
    let symbol = symbol_map.find_symbol(pos)?;
    Some(*symbol.define_loc())
}

#[cfg(test)]
mod tests {
    use crate::tests;

    fn check(s: &str) -> String {
        let (db, f) = tests::single_file(s);
        let definition = super::exec(&db, f.marker(0)).expect("definition not found");
        let content = f.file_content(&f.root_file());
        content[definition.range].to_string()
    }

    #[test]
    fn class() {
        insta::assert_debug_snapshot!(check("class Foo; class $Bar : Foo;"));
    }

    #[test]
    fn class_template_arg() {
        insta::assert_debug_snapshot!(check("class Foo<int $foo>;"));
    }

    #[test]
    fn class_parent() {
        insta::assert_debug_snapshot!(check("class Foo; class Bar : $Foo;"));
    }

    #[test]
    fn class_field() {
        insta::assert_debug_snapshot!(check("class Foo {int $foo}"));
    }

    #[test]
    fn multiple_files() {
        let (db, f) = tests::multiple_files(
            r#"
; main.td
include "sub.td"
class Foo : $Bar;

; sub.td
class Bar;
            "#,
        );
        let symbols = super::exec(&db, f.marker(0));
        insta::assert_debug_snapshot!(symbols);
    }
}
