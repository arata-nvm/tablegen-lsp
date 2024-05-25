use crate::eval::EvalDatabase;
use crate::file_system::{FilePosition, FileRange};

pub fn exec(db: &dyn EvalDatabase, pos: FilePosition) -> Option<Vec<FileRange>> {
    let evaluation = db.eval();
    let symbol_map = evaluation.symbol_map();
    let symbol = symbol_map.find_symbol_at(pos)?;
    Some(symbol.reference_locs().to_vec())
}

#[cfg(test)]
mod tests {
    use crate::tests;

    fn check(s: &str) -> Vec<String> {
        let (db, f) = tests::single_file(s);
        let references = super::exec(&db, f.marker(0)).expect("definition not found");
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
