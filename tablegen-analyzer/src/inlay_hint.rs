use ecow::EcoString;

use tablegen_parser::ast::{AstNode, ClassRef, ClassValue};
use tablegen_parser::parser::{TextRange, TextSize};
use tablegen_parser::syntax_kind::SyntaxKind;

use crate::document::Document;
use crate::symbol::{Record, RecordFieldKind, RecordKind, Symbol};

#[derive(Debug)]
pub struct InlayHint {
    pub position: TextSize,
    pub label: String,
    pub kind: InlayHintKind,
}

#[derive(Debug)]
pub enum InlayHintKind {
    TemplateArg,
    FieldLet,
}

impl InlayHint {
    fn new(position: TextSize, label: impl Into<String>, kind: InlayHintKind) -> Self {
        Self {
            position,
            label: label.into(),
            kind,
        }
    }
}

pub fn inlay_hint(doc: &Document, range: TextRange) -> Option<Vec<InlayHint>> {
    let mut hints = vec![];
    for (symbol_range, symbol) in doc.symbol_map().get_symbols_in(range) {
        match symbol {
            Symbol::Record(record) if matches!(record.kind(), RecordKind::Class) => {
                let symbol_range = TextRange::new(symbol_range.start, symbol_range.end);
                if let Some(new_hints) = inlay_hint_class(doc, symbol_range, record) {
                    hints.extend(new_hints);
                }
            }
            Symbol::RecordField(field) if matches!(field.kind(), RecordFieldKind::FieldLet) => {
                hints.push(InlayHint::new(
                    symbol_range.end,
                    format!(":{}", field.r#type()),
                    InlayHintKind::FieldLet,
                ));
            }
            _ => {}
        };
    }
    Some(hints)
}

fn inlay_hint_class(doc: &Document, range: TextRange, record: &Record) -> Option<Vec<InlayHint>> {
    let mut hints = vec![];
    let template_arg_names: Vec<EcoString> = record
        .template_args()
        .into_iter()
        .filter_map(|symbol_id| doc.symbol_map().symbol(symbol_id))
        .map(|arg| arg.name().clone())
        .collect();

    let id_node = doc.root().covering_element(range);
    let identifier_node = match id_node.kind() {
        SyntaxKind::Id => id_node.parent()?,
        SyntaxKind::Identifier => id_node.into_node()?,
        _ => return None,
    };

    let class_node = identifier_node.parent()?;
    let arg_list = match class_node.kind() {
        SyntaxKind::ClassRef => {
            let class_ref = ClassRef::cast(class_node)?;
            class_ref.arg_value_list()?
        }
        SyntaxKind::ClassValue => {
            let class_value = ClassValue::cast(class_node)?;
            class_value.arg_value_list()?
        }
        _ => return None,
    };

    let positional = arg_list.positional()?;
    for (value, name) in positional.values().zip(template_arg_names) {
        let value_range = value.syntax().text_range();
        hints.push(InlayHint::new(
            value_range.start(),
            format!("{}:", name),
            InlayHintKind::TemplateArg,
        ));
    }

    Some(hints)
}
