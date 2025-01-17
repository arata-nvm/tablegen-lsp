use syntax::{
    ast::{self, AstNode},
    parser::TextRange,
    SyntaxNodePtr,
};

use crate::{
    file_system::{FileId, IncludeId},
    index::IndexDatabase,
};

pub fn exec(db: &dyn IndexDatabase, file_id: FileId) -> Option<Vec<DocumentLink>> {
    let include_map = db.resolved_include_map(file_id);
    let parse = db.parse(file_id);
    let root_node = parse.syntax_node();
    let links = root_node
        .descendants()
        .filter_map(|node| {
            let include = ast::Include::cast(node)?;
            let include_id = IncludeId(SyntaxNodePtr::new(include.syntax()));
            let range = include.path()?.syntax().text_range();
            let target = *include_map.get(&include_id)?;
            Some(DocumentLink { range, target })
        })
        .collect();
    Some(links)
}

#[derive(Debug)]
pub struct DocumentLink {
    pub range: TextRange,
    pub target: FileId,
}

#[cfg(test)]
mod tests {
    use crate::tests;

    use super::DocumentLink;

    fn check(s: &str) -> Vec<DocumentLink> {
        let (db, f) = tests::multiple_files(s);
        super::exec(&db, f.root_file()).unwrap_or_default()
    }

    #[test]
    fn include_not_found() {
        insta::assert_debug_snapshot!(check(
            r#"
; file1.id
include "file2.td";
        "#
        ));
    }

    #[test]
    fn include_found() {
        insta::assert_debug_snapshot!(check(
            r#"
; file1.td
include "file2.td";

; file2.td
class Foo;
    "#
        ));
    }
}
