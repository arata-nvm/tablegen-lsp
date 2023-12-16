use tablegen_parser::ast;
use tablegen_parser::ast::AstNode;
use tablegen_parser::parser::TextSize;
use tablegen_parser::syntax_kind::SyntaxKind;

use crate::document::Document;
use crate::symbol::{RecordKind, Symbol, VariableKind};

#[derive(Eq, PartialEq, Hash)]
pub struct CompletionItem {
    pub label: String,
    pub detail: String,
    pub kind: CompletionItemKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompletionItemKind {
    Keyword,
    Type,
    Class,
    Def,
    Defset,
    Defvar,
    Field,
    TemplateArg,
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

    let root = doc.root();
    if let Some(cur_token) = root.token_at_offset(pos).left_biased() {
        if let Some(parent_node) = cur_token.parent() {
            if let Some(parent_parent_node) = parent_node.parent() {
                if parent_parent_node.kind() == SyntaxKind::StatementList {
                    // A
                    const KEYWORDS: [&str; 12] = [
                        "assert",
                        "class",
                        "def",
                        "dump",
                        "foreach",
                        "defm",
                        "defset",
                        "defvar",
                        "if",
                        "include",
                        "let",
                        "multiclass",
                    ];
                    for keyword in KEYWORDS {
                        items.push(CompletionItem::new(
                            keyword,
                            "",
                            CompletionItemKind::Keyword,
                        ));
                    }
                }
                if ast::Type::can_cast(parent_parent_node.kind()) {
                    // B
                    const PRIMITIVE_TYPES: [&str; 7] =
                        ["bit", "bits", "code", "dag", "int", "list", "string"];
                    for typ in PRIMITIVE_TYPES {
                        items.push(CompletionItem::new(typ, "", CompletionItemKind::Type));
                    }
                }
                if parent_parent_node.kind() == SyntaxKind::Value
                    || parent_parent_node.kind() == SyntaxKind::InnerValue
                {
                    // C
                    const BOOLEAN_VALUES: [&str; 2] = ["false", "true"];
                    for value in BOOLEAN_VALUES {
                        items.push(CompletionItem::new(value, "", CompletionItemKind::Keyword));
                    }

                    for symbol_id in doc.symbol_map().global_symbols() {
                        let Some(symbol) = doc.symbol_map().symbol(*symbol_id) else {
                            continue;
                        };

                        match symbol {
                            Symbol::Record(record) => match record.kind() {
                                RecordKind::Def => items.push(CompletionItem::new(
                                    record.name(),
                                    "def",
                                    CompletionItemKind::Def,
                                )),
                                _ => {}
                            },
                            Symbol::RecordField(_) => {}
                            Symbol::Variable(variable) => match variable.kind() {
                                VariableKind::Defset => items.push(CompletionItem::new(
                                    variable.name(),
                                    variable.r#type().to_string(),
                                    CompletionItemKind::Defset,
                                )),
                                VariableKind::Defvar => items.push(CompletionItem::new(
                                    variable.name(),
                                    variable.r#type().to_string(),
                                    CompletionItemKind::Defvar,
                                )),
                                VariableKind::Temporary => {}
                            },
                        }
                    }
                }
            }
        }
    }

    Some(items)
}
