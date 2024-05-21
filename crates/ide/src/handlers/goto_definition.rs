use crate::eval::EvalDatabase;
use crate::file_system::{FilePosition, FileRange};

pub fn exec(db: &dyn EvalDatabase, pos: FilePosition) -> Option<FileRange> {
    let evaluation = db.eval();
    let symbol_map = evaluation.symbol_map();
    let symbol = symbol_map.find_symbol_at(pos)?;
    Some(*symbol.define_loc())
}

#[cfg(test)]
mod tests {
    use crate::tests;

    #[test]
    fn myself() {
        let (db, f) = tests::single_file("class Foo; class $Bar : Foo;");
        let symbols = super::exec(&db, f.marker(0));
        insta::assert_debug_snapshot!(symbols);
    }

    #[test]
    fn other() {
        let (db, f) = tests::single_file("class Foo; class Bar : $Foo;");
        let symbols = super::exec(&db, f.marker(0));
        insta::assert_debug_snapshot!(symbols);
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
