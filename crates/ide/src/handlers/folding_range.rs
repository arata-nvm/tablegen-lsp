use syntax::{parser::TextRange, syntax_kind::SyntaxKind, SyntaxNode};

use crate::{db::SourceDatabase, file_system::FileId};

pub fn exec(db: &dyn SourceDatabase, file_id: FileId) -> Option<Vec<FoldingRange>> {
    let parse = db.parse(file_id);
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
            | SyntaxKind::MultiClass => Some(folding_range(&node)),
            _ => None,
        })
        .map(|range| FoldingRange { range })
        .collect();
    Some(ranges)
}

fn folding_range(node: &SyntaxNode) -> TextRange {
    let start = node.text_range().start();
    let mut end_token = node.last_token();
    while let Some(token) = end_token {
        if !token.kind().is_trivia() {
            return TextRange::new(start, token.text_range().end());
        }
        end_token = token.prev_token();
    }
    TextRange::empty(start)
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
