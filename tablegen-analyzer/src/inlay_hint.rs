use ecow::EcoString;

use tablegen_parser::ast::{AstNode, ClassRef};
use tablegen_parser::parser::{TextRange, TextSize};
use tablegen_parser::syntax_kind::SyntaxKind;

use crate::document::Document;
use crate::symbol::{Record, RecordKind, Symbol};

pub struct InlayHint {
    pub position: TextSize,
    pub label: String,
}

impl InlayHint {
    fn new(position: TextSize, label: impl Into<String>) -> Self {
        Self {
            position,
            label: label.into(),
        }
    }
}

pub fn inlay_hint(doc: &Document, range: TextRange) -> Option<Vec<InlayHint>> {
    let mut hints = vec![];
    for (symbol_range, symbol_id) in doc.symbol_map().get_symbols_in(range) {
        let Some(symbol) = doc.symbol_map().symbol(symbol_id) else {
            continue;
        };

        match symbol {
            Symbol::Record(record) if matches!(record.kind(), RecordKind::Class) => {
                let symbol_range = TextRange::new(symbol_range.start, symbol_range.end);
                if let Some(new_hints) = inlay_hint_class(doc, symbol_range, record) {
                    hints.extend(new_hints);
                }
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
    let class_ref_node = identifier_node.parent()?;
    let class_ref = ClassRef::cast(class_ref_node)?;

    let arg_list = class_ref.arg_value_list()?;
    let positional = arg_list.positional()?;
    for (value, name) in positional.values().zip(template_arg_names) {
        let value_range = value.syntax().text_range();
        hints.push(InlayHint::new(value_range.start(), format!("{}:", name)));
    }

    Some(hints)
}
