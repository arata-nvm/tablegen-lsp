use tablegen_parser::{kind::TokenKind, linked_node::LinkedNode, node::SyntaxNode, parser::Range};

use crate::symbol::{RecordFieldKind, RecordKind, Symbol};

pub fn hover(symbol: &Symbol, root: &SyntaxNode) -> Option<String> {
    let (_, range) = symbol.define_loc().clone();
    let node = LinkedNode::new(root);

    let symbol_info = extract_symbol_info(symbol);
    let symbol_doc = extract_doc_comments(range, node);

    let mut hover = String::new();
    hover.push_str(&symbol_info);
    if let Some(symbol_doc) = symbol_doc {
        hover.push_str(&symbol_doc);
    }
    Some(hover)
}

fn extract_symbol_info(symbol: &Symbol) -> String {
    match symbol {
        Symbol::Record(record) => {
            let name = match record.kind() {
                RecordKind::Class => format!("**class** `{}`", record.name()),
                RecordKind::Def => format!("**def** `{}`", record.name()),
            };
            format!("{name}\n***\n")
        }
        Symbol::RecordField(record_field) => {
            let name = match record_field.kind() {
                RecordFieldKind::TemplateArg => {
                    format!("**template arg** `{}`", record_field.name())
                }
                RecordFieldKind::Field => format!("**field** `{}`", record_field.name()),
            };
            let typ = record_field.r#type();
            format!("{name}\n***\nType: `{typ}`\n***\n")
        }
    }
}

fn extract_doc_comments(range: Range, node: LinkedNode<'_>) -> Option<String> {
    let id_node = node.find(range)?;
    let parent_node = id_node.parent()?;

    let mut sibling = parent_node.prev_sibling()?;
    let mut comments = Vec::new();
    loop {
        if sibling.node().token_kind() != TokenKind::Whitespace
            || sibling.node().text().matches("\n").count() != 1
        {
            break;
        }

        sibling = sibling.prev_sibling()?;
        if sibling.node().token_kind() != TokenKind::LineComment {
            break;
        }

        let comment = sibling.node().text();
        if !comment.starts_with("//") {
            break;
        }
        let comment_content = comment.trim_start_matches('/').trim_start().to_string();
        comments.push(comment_content);
        sibling = sibling.prev_sibling()?;
    }

    comments
        .into_iter()
        .rev()
        .collect::<Vec<_>>()
        .join("\n")
        .into()
}
