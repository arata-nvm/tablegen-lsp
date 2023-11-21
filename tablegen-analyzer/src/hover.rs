use tablegen_parser::{language::SyntaxNode, parser::TextRange, syntax_kind::SyntaxKind};

use crate::symbol::{RecordFieldKind, RecordKind, Symbol};

pub fn hover(symbol: &Symbol, root: SyntaxNode) -> Option<String> {
    let (_, range) = symbol.define_loc().clone();

    let symbol_info = extract_symbol_info(symbol);
    let symbol_doc = extract_doc_comments(range, root);

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

fn extract_doc_comments(range: TextRange, root: SyntaxNode) -> Option<String> {
    let id_node = root.covering_element(range);
    let identifier_node = match id_node.kind() {
        SyntaxKind::Id => id_node.parent()?,
        SyntaxKind::Identifier => id_node.into_node()?,
        _ => return None,
    };

    // Class or FieldDef
    let parent_node = identifier_node.parent()?;

    let mut sibling = parent_node.prev_sibling_or_token()?.into_token()?;
    let mut comments = Vec::new();
    loop {
        if sibling.kind() != SyntaxKind::Whitespace || !sibling.text().contains('\n') {
            break;
        }

        sibling = match sibling
            .prev_sibling_or_token()
            .and_then(|elm| elm.into_token())
        {
            Some(sibling) => sibling,
            None => break,
        };

        if sibling.kind() != SyntaxKind::LineComment {
            break;
        }

        let comment = sibling.text();
        if !comment.starts_with("//") {
            break;
        }
        let comment_content = comment.trim_start_matches('/').trim_start().to_string();
        comments.push(comment_content);

        sibling = match sibling
            .prev_sibling_or_token()
            .and_then(|elm| elm.into_token())
        {
            Some(sibling) => sibling,
            None => break,
        };
    }

    comments
        .into_iter()
        .rev()
        .collect::<Vec<_>>()
        .join("\n")
        .into()
}
