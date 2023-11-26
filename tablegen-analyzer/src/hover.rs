use tablegen_parser::{language::SyntaxNode, parser::TextRange, syntax_kind::SyntaxKind};

use crate::symbol::{RecordFieldKind, RecordKind, Symbol, VariableKind};

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
        Symbol::Variable(variable) => {
            let name = match variable.kind() {
                VariableKind::Defset => format!("**defset** `{}`", variable.name()),
                VariableKind::Defvar => format!("**defvar** `{}`", variable.name()),
                VariableKind::Temporary => format!("**temporary var** `{}`", variable.name()),
            };
            let typ = variable.r#type();
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

    // Class or FieldDef or Defset or InnerValue
    let mut parent_node = identifier_node.parent()?;

    if parent_node.kind() == SyntaxKind::InnerValue {
        let value_node = parent_node.parent()?;
        // Def
        parent_node = value_node.parent()?;
    }

    let mut cur_token = parent_node.first_token()?;
    let mut comments = Vec::new();
    loop {
        cur_token = match cur_token.prev_token() {
            Some(t) => t,
            None => break,
        };
        if cur_token.kind() != SyntaxKind::Whitespace || cur_token.text().matches('\n').count() != 1
        {
            break;
        }

        cur_token = match cur_token.prev_token() {
            Some(t) => t,
            None => break,
        };
        if cur_token.kind() != SyntaxKind::LineComment {
            break;
        }

        let comment = cur_token.text();
        if !comment.starts_with("//") {
            break;
        }
        comments.push(comment.trim_start_matches('/').trim_start().to_string());
    }

    comments
        .into_iter()
        .rev()
        .collect::<Vec<_>>()
        .join("\n")
        .into()
}
