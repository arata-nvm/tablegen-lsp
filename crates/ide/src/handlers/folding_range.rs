use syntax::{parser::TextRange, syntax_kind::SyntaxKind};

use crate::{
    db::{Db, parse},
    file_system::FileId,
    utils,
};

pub fn exec(db: &dyn Db, file_id: FileId) -> Option<Vec<FoldingRange>> {
    let parse = parse(db, file_id);
    let root_node = parse.syntax_node();
    let ranges = root_node
        .descendants()
        .filter_map(|node| match node.kind() {
            SyntaxKind::Class
            | SyntaxKind::Def
            | SyntaxKind::Defset
            | SyntaxKind::Foreach
            | SyntaxKind::If
            | SyntaxKind::Let
            | SyntaxKind::MultiClass => Some(utils::range_excluding_trivia(&node)),
            _ => None,
        })
        .map(|range| FoldingRange { range })
        .collect();
    Some(ranges)
}

#[derive(Debug)]
pub struct FoldingRange {
    pub range: TextRange,
}

#[cfg(test)]
mod tests {
    use crate::tests;

    use super::FoldingRange;

    fn check(s: &str) -> Option<Vec<FoldingRange>> {
        let (db, f) = tests::single_file(s);
        super::exec(&db, f.root_file())
    }

    #[test]
    fn class() {
        insta::assert_debug_snapshot!(check(
            r#"
class Foo {
  int a;
}

class Bar;
"#
        ));
    }

    #[test]
    fn multiclass() {
        insta::assert_debug_snapshot!(check(
            r#"
multiclass Foo {
    def foo {
	    int a;
    }
}
"#
        ));
    }
}
