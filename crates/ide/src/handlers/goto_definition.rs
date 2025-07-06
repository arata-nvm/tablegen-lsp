use crate::{
    db::Db,
    file_system::{FilePosition, FileRange, SourceUnitId},
    index::index,
};

pub fn exec(db: &dyn Db, source_unit_id: SourceUnitId, pos: FilePosition) -> Option<FileRange> {
    let index = index(db, source_unit_id);
    let symbol_map = index.symbol_map();
    let symbol = symbol_map.find_symbol_at(pos)?;
    Some(*symbol.define_loc())
}

#[cfg(test)]
mod tests {
    use crate::tests;

    fn check(s: &str) -> String {
        let (db, f) = tests::single_file(s);
        let definition =
            super::exec(&db, f.source_unit_id(), f.marker(0)).expect("definition not found");
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
    fn class_parent_arg() {
        insta::assert_debug_snapshot!(check("class Foo<int x>; class Bar<int y> : Foo<$y>;"));
    }

    #[test]
    fn class_field() {
        insta::assert_debug_snapshot!(check("class Foo {int $foo}"));
    }

    #[test]
    fn def() {
        insta::assert_debug_snapshot!(check("def $foo;"));
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
        let symbols = super::exec(&db, f.source_unit_id(), f.marker(0));
        insta::assert_debug_snapshot!(symbols);
    }
}
