use crate::{
    file_system::{FilePosition, FileRange, SourceUnitId},
    index::{Index, IndexDatabase},
};

pub fn exec(
    db: &dyn IndexDatabase,
    source_unit_id: SourceUnitId,
    pos: FilePosition,
) -> Option<Vec<FileRange>> {
    let index = db.index(source_unit_id);
    exec_with_index(&index, pos)
}

/// Execute reference search using a pre-computed `Index`.
///
/// This allows callers (such as the LSP layer) to reuse a background-computed
/// index snapshot instead of re-running the `Index` salsa query on the hot
/// request path.
pub fn exec_with_index(index: &Index, pos: FilePosition) -> Option<Vec<FileRange>> {
    let symbol_map = index.symbol_map();
    let symbol_id = symbol_map.find_symbol_at(pos)?;
    let symbol = symbol_map.symbol(symbol_id);
    Some(symbol.reference_locs().to_vec())
}

#[cfg(test)]
mod tests {
    use crate::tests;

    fn check(s: &str) -> Vec<String> {
        let (db, f) = tests::single_file(s);
        let references =
            super::exec(&db, f.source_unit_id(), f.marker(0)).expect("definition not found");
        let content = f.file_content(&f.root_file());
        references
            .into_iter()
            .map(|it| content[it.range].to_string())
            .collect()
    }

    #[test]
    fn class() {
        insta::assert_debug_snapshot!(check("class $Foo; class Bar: Foo;"));
        insta::assert_debug_snapshot!(check("class $Foo { Foo foo; }"));
    }

    #[test]
    fn template_arg() {
        insta::assert_debug_snapshot!(check("class Foo<int $arg1> { int field1 = arg1; }"));
    }

    #[test]
    fn field() {
        insta::assert_debug_snapshot!(check("class Foo { int $field1 = 1; int field2 = field1; }"));
    }
}
