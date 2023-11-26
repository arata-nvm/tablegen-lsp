use tablegen_parser::ast::{AstNode, Class};
use tablegen_parser::parser::TextSize;
use tablegen_parser::syntax_kind::SyntaxKind;

use crate::document::Document;
use crate::symbol::{RecordKind, Symbol, VariableKind};
use crate::symbol_map::SymbolMap;

pub struct CompletionItem {
    pub label: String,
    pub detail: String,
    pub kind: CompletionItemKind,
}

pub enum CompletionItemKind {
    Keyword,
    Type,
    Class,
    Def,
    Defset,
    Defvar,
    Field,
}

impl CompletionItem {
    fn new(label: impl Into<String>, detail: impl Into<String>, kind: CompletionItemKind) -> Self {
        Self {
            label: label.into(),
            detail: detail.into(),
            kind,
        }
    }
}

pub fn completion(doc: &Document, pos: TextSize) -> Option<Vec<CompletionItem>> {
    let mut items = Vec::new();
    complete_keyword(&mut items);
    complete_type(&mut items);
    complete_symbol(doc.symbol_map(), &mut items);
    complete_scoped_symbol(doc, pos, &mut items);
    Some(items)
}

fn complete_keyword(items: &mut Vec<CompletionItem>) {
    const KEYWORDS: [&str; 18] = [
        "assert",
        "class",
        "def",
        "dump",
        "else",
        "false",
        "foreach",
        "defm",
        "defset",
        "defvar",
        "field",
        "if",
        "in",
        "include",
        "let",
        "multiclass",
        "then",
        "true",
    ];

    for keyword in KEYWORDS {
        items.push(CompletionItem::new(
            keyword,
            "",
            CompletionItemKind::Keyword,
        ));
    }
}

fn complete_type(items: &mut Vec<CompletionItem>) {
    const TYPES: [&str; 7] = ["bit", "bits", "code", "dag", "int", "list", "string"];

    for typ in TYPES {
        items.push(CompletionItem::new(typ, "", CompletionItemKind::Type))
    }
}

fn complete_symbol(symbol_map: &SymbolMap, items: &mut Vec<CompletionItem>) {
    for symbol_id in symbol_map.global_symbols() {
        let Some(symbol) = symbol_map.symbol(*symbol_id) else {
            continue;
        };

        match symbol {
            Symbol::Record(record) => match record.kind() {
                RecordKind::Class => items.push(CompletionItem::new(
                    record.name().as_str(),
                    "class",
                    CompletionItemKind::Class,
                )),
                RecordKind::Def => items.push(CompletionItem::new(
                    record.name().as_str(),
                    "def",
                    CompletionItemKind::Def,
                )),
            },
            Symbol::RecordField(_) => {}
            Symbol::Variable(variable) => match variable.kind() {
                VariableKind::Defset => items.push(CompletionItem::new(
                    variable.name().as_str(),
                    variable.r#type().to_string(),
                    CompletionItemKind::Defset,
                )),
                VariableKind::Defvar => items.push(CompletionItem::new(
                    variable.name().as_str(),
                    variable.r#type().to_string(),
                    CompletionItemKind::Defvar,
                )),
                VariableKind::Temporary => {}
            },
        }
    }
}

fn complete_scoped_symbol(
    doc: &Document,
    pos: TextSize,
    items: &mut Vec<CompletionItem>,
) -> Option<()> {
    let root = doc.root();
    let cur_token = root.token_at_offset(pos).left_biased()?;

    let mut cur_node = cur_token.parent()?;
    loop {
        match cur_node.kind() {
            SyntaxKind::Class => {
                let class = Class::cast(cur_node.clone())?;
                let name_range = class.name()?.syntax().text_range();
                let symbol = doc.symbol_map().get_symbol_at(name_range.start())?;
                let field_ids = doc.symbol_map().get_all_fields(symbol);
                let field_items = field_ids
                    .into_iter()
                    .filter_map(|field_id| doc.symbol_map().symbol(field_id))
                    .filter_map(|symbol| symbol.as_field())
                    .map(|field| {
                        CompletionItem::new(
                            field.name(),
                            field.r#type().to_string(),
                            CompletionItemKind::Field,
                        )
                    });
                items.extend(field_items);
                break;
            }
            _ => {}
        }
        cur_node = cur_node.parent()?;
    }

    None
}
