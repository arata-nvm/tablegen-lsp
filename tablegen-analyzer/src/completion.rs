use tablegen_parser::parser::TextSize;

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
}

impl CompletionItem {
    fn new<S: Into<String>>(label: S, detail: S, kind: CompletionItemKind) -> Self {
        Self {
            label: label.into(),
            detail: detail.into(),
            kind,
        }
    }
}

pub fn completion(_pos: TextSize, symbol_map: &SymbolMap) -> Option<Vec<CompletionItem>> {
    let mut items = Vec::new();
    complete_keyword(&mut items);
    complete_type(&mut items);
    complete_symbol(symbol_map, &mut items);
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
                    "defset",
                    CompletionItemKind::Defset,
                )),
                _ => {}
            },
        }
    }
}
