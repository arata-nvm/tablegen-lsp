pub mod analyze;
pub mod completion;
pub mod config;
pub mod document;
pub mod document_map;
pub mod hover;
pub mod indexer;
pub mod inlay_hint;
pub mod server_impl;
pub mod source;
pub mod symbol;
pub mod symbol_map;

#[cfg(test)]
mod tests {
    use tablegen_parser::parser::{TextRange, TextSize};

    use crate::document::{Document, DocumentId};

    fn pos(doc: &Document, line: usize, char_: usize) -> TextSize {
        doc.line_to_pos(line).unwrap() + TextSize::try_from(char_).unwrap()
    }

    #[test]
    fn definition() {
        let text = include_str!("../testdata/simple.td");
        let doc = Document::parse(DocumentId::new(0), text);

        // class Foo
        assert_eq!(
            doc.get_definition(pos(&doc, 1, 6)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 1, 6), pos(&doc, 1, 9)),
            )),
        );
        assert_eq!(
            doc.get_definition(pos(&doc, 4, 12)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 1, 6), pos(&doc, 1, 9)),
            ))
        );
        assert_eq!(
            doc.get_definition(pos(&doc, 6, 12)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 1, 6), pos(&doc, 1, 9)),
            ))
        );
        assert_eq!(
            doc.get_definition(pos(&doc, 7, 12)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 1, 6), pos(&doc, 1, 9)),
            ))
        );
        assert_eq!(
            doc.get_definition(pos(&doc, 12, 9)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 1, 6), pos(&doc, 1, 9)),
            ))
        );

        // arg a
        assert_eq!(
            doc.get_definition(pos(&doc, 1, 14)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 1, 14), pos(&doc, 1, 15)),
            ))
        );
        assert_eq!(
            doc.get_definition(pos(&doc, 2, 10)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 1, 14), pos(&doc, 1, 15)),
            ))
        );

        // field A
        assert_eq!(
            doc.get_definition(pos(&doc, 2, 6)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 2, 6), pos(&doc, 2, 7)),
            ))
        );

        // class Bar
        assert_eq!(
            doc.get_definition(pos(&doc, 4, 6)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 4, 6), pos(&doc, 4, 9)),
            ))
        );

        // defset foos
        assert_eq!(
            doc.get_definition(pos(&doc, 6, 17)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 6, 17), pos(&doc, 6, 21)),
            ))
        );

        // def foo1
        assert_eq!(
            doc.get_definition(pos(&doc, 7, 6)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 7, 6), pos(&doc, 7, 10)),
            ))
        );

        // def foo2
        assert_eq!(
            doc.get_definition(pos(&doc, 8, 6)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 8, 6), pos(&doc, 8, 10)),
            ))
        );

        // def foo
        assert_eq!(
            doc.get_definition(pos(&doc, 12, 4)),
            Some((
                DocumentId::new(0),
                TextRange::new(pos(&doc, 12, 4), pos(&doc, 12, 7)),
            ))
        );
    }

    #[test]
    fn reference() {
        let text = include_str!("../testdata/simple.td");
        let doc = Document::parse(DocumentId::new(0), text);

        // class Foo
        assert_eq!(
            doc.get_references(pos(&doc, 1, 6)),
            Some(vec![
                (
                    DocumentId::new(0),
                    TextRange::new(pos(&doc, 4, 12), pos(&doc, 4, 15))
                ),
                (
                    DocumentId::new(0),
                    TextRange::new(pos(&doc, 6, 12), pos(&doc, 6, 15))
                ),
                (
                    DocumentId::new(0),
                    TextRange::new(pos(&doc, 7, 12), pos(&doc, 7, 15))
                ),
                (
                    DocumentId::new(0),
                    TextRange::new(pos(&doc, 12, 9), pos(&doc, 12, 12))
                ),
            ]),
        );

        // arg a
        assert_eq!(
            doc.get_references(pos(&doc, 1, 14)),
            Some(vec![(
                DocumentId::new(0),
                TextRange::new(pos(&doc, 2, 10), pos(&doc, 2, 11)),
            )])
        );

        // field A
        assert_eq!(doc.get_references(pos(&doc, 2, 6)), Some(vec![]));

        // class Bar
        assert_eq!(
            doc.get_references(pos(&doc, 4, 6)),
            Some(vec![(
                DocumentId::new(0),
                TextRange::new(pos(&doc, 8, 12), pos(&doc, 8, 15))
            )])
        );

        // defset foos
        assert_eq!(doc.get_references(pos(&doc, 6, 17)), Some(vec![]));

        // def foo1
        assert_eq!(doc.get_references(pos(&doc, 7, 6)), Some(vec![]));

        // def foo2
        assert_eq!(doc.get_references(pos(&doc, 8, 6)), Some(vec![]));

        // def foo
        assert_eq!(doc.get_references(pos(&doc, 12, 4)), Some(vec![]));
    }

    #[test]
    fn hover() {
        let text = include_str!("../testdata/simple.td");
        let doc = Document::parse(DocumentId::new(0), text);

        // class Foo
        assert_eq!(
            doc.get_hover(pos(&doc, 1, 6)),
            Some("**class** `Foo`\n***\nThis is a document.".into())
        );
        assert_eq!(
            doc.get_hover(pos(&doc, 4, 12)),
            Some("**class** `Foo`\n***\nThis is a document.".into()),
        );

        // arg a
        assert_eq!(
            doc.get_hover(pos(&doc, 1, 14)),
            Some("**template arg** `a`\n***\nType: `int`\n***\n".into())
        );

        // field A
        assert_eq!(
            doc.get_hover(pos(&doc, 2, 6)),
            Some("**field** `A`\n***\nType: `int`\n***\n".into())
        );

        // class Bar
        assert_eq!(
            doc.get_hover(pos(&doc, 4, 6)),
            Some("**class** `Bar`\n***\n".into())
        );

        // defset foos
        assert_eq!(
            doc.get_hover(pos(&doc, 6, 17)),
            Some("**defset** `foos`\n***\nType: `list<Foo>`\n***\n".into())
        );

        // def foo1
        assert_eq!(
            doc.get_hover(pos(&doc, 7, 6)),
            Some("**def** `foo1`\n***\n".into())
        );

        // def foo2
        assert_eq!(
            doc.get_hover(pos(&doc, 8, 6)),
            Some("**def** `foo2`\n***\n".into())
        );

        // def foo
        assert_eq!(
            doc.get_hover(pos(&doc, 12, 4)),
            Some("**def** `foo`\n***\ndocument 2".into()),
        );
    }

    #[test]
    fn empty() {
        let doc = Document::parse(DocumentId::new(0), "");
        let result = doc.get_inlay_hint(TextRange::empty(TextSize::new(0)));
        assert!(result.is_none());
    }
}
