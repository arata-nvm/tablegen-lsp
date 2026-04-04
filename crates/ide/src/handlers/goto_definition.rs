use crate::{
    db::IndexDatabase,
    file_system::{FilePosition, FileRange, SourceUnitId},
    index::Index,
};

pub fn exec(
    db: &dyn IndexDatabase,
    source_unit_id: SourceUnitId,
    pos: FilePosition,
) -> Option<FileRange> {
    let Index { symbol_map, .. } = &*db.index(source_unit_id);
    let symbol_id = symbol_map.find_symbol_at(pos)?;
    let symbol = symbol_map.symbol(symbol_id);
    Some(*symbol.define_loc())
}

#[cfg(test)]
mod tests {
    use crate::tests;

    fn check(s: &str) -> String {
        let (db, f) = tests::single_file(s);
        let definition =
            super::exec(&db, f.source_unit_id(), f.marker(0)).expect("definition not found");
        tests::render_file_range_block(&f, definition, std::iter::empty::<&str>())
    }

    fn check_multi(s: &str) -> String {
        let (db, f) = tests::multiple_files(s);
        let definition =
            super::exec(&db, f.source_unit_id(), f.marker(0)).expect("definition not found");
        tests::render_file_range_block(&f, definition, std::iter::empty::<&str>())
    }

    #[test]
    fn class() {
        insta::assert_snapshot!(check("class Foo; class $Bar : Foo;"));
    }

    #[test]
    fn class_template_arg() {
        insta::assert_snapshot!(check("class Foo<int $foo>;"));
    }

    #[test]
    fn class_parent() {
        insta::assert_snapshot!(check("class Foo; class Bar : $Foo;"));
    }

    #[test]
    fn class_parent_arg() {
        insta::assert_snapshot!(check("class Foo<int x>; class Bar<int y> : Foo<$y>;"));
    }

    #[test]
    fn class_field() {
        insta::assert_snapshot!(check("class Foo {int $foo}"));
    }

    #[test]
    fn def() {
        insta::assert_snapshot!(check("def $foo;"));
    }

    #[test]
    fn multiple_files() {
        insta::assert_snapshot!(check_multi(
            r#"
; main.td
include "sub.td"
class Foo : $Bar;

; sub.td
class Bar;
            "#,
        ));
    }
}
