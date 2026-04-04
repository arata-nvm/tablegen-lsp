use syntax::{
    SyntaxNodePtr,
    ast::{self, AstNode},
    parser::TextRange,
};

use crate::{
    db::IndexDatabase,
    file_system::{FileId, IncludeId, SourceUnitId},
    utils,
};

pub fn exec(
    db: &dyn IndexDatabase,
    source_unit_id: SourceUnitId,
    file_id: FileId,
) -> Option<Vec<DocumentLink>> {
    let source_unit = db.source_unit(source_unit_id).source_unit(db);
    let include_map = source_unit.include_map(&file_id)?;
    let parse = db.parse(file_id);
    let root_node = parse.syntax_node();
    let links = root_node
        .descendants()
        .filter_map(|node| {
            let include = ast::Include::cast(node)?;
            let include_id = IncludeId(SyntaxNodePtr::new(include.syntax()));
            let range = utils::range_excluding_trivia(include.path()?.syntax());
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
    use crate::file_system::{FileRange, FileSystem};
    use crate::tests;

    fn check(s: &str) -> String {
        let (db, f) = tests::multiple_files(s);
        let links = super::exec(&db, f.source_unit_id(), f.root_file()).unwrap();

        let mut out = String::new();
        for link in links {
            let range = FileRange::new(f.root_file(), link.range);
            let target = f.path_for_file(&link.target);
            out.push_str(&tests::render_file_range_block(
                &f,
                range,
                [format!("target: {target}")],
            ));
            out.push('\n');
        }
        out
    }

    #[test]
    fn include_not_found() {
        insta::assert_snapshot!(check(
            r#"
; file1.id
include "file2.td";
        "#
        ));
    }

    #[test]
    fn include_found() {
        insta::assert_snapshot!(check(
            r#"
; file1.td
include "file2.td";

; file2.td
class Foo;
    "#
        ));
    }
}
