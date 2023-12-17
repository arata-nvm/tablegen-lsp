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
                        if symbol.define_loc().1.start() >= pos {
                            continue;
                        }

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

                    if let Some(class) = parent_node
                        .ancestors()
                        .find(|node| node.kind() == SyntaxKind::Class)
                        .and_then(|node| ast::Class::cast(node))
                    {
                        if let Some(name_range) = class.name().and_then(|name| name.range()) {
                            if let Some(class_symbol) =
                                doc.symbol_map().get_symbol_at(name_range.start())
                            {
                                if let Some(record) = class_symbol.as_record() {
                                    let num_args_to_complete = class
                                        .template_arg_list()
                                        .and_then(|list| {
                                            list.args()
                                                .map(|arg| arg.syntax().text_range())
                                                .position(|range| range.contains_inclusive(pos))
                                        })
                                        .unwrap_or(usize::MAX);
                                    let new_items = record
                                        .template_args()
                                        .into_iter()
                                        .filter_map(|symbol_id| doc.symbol_map().symbol(symbol_id))
                                        .filter_map(|symbol| symbol.as_field())
                                        .take(num_args_to_complete)
                                        .map(|field| {
                                            CompletionItem::new(
                                                field.name(),
                                                field.r#type().to_string(),
                                                CompletionItemKind::TemplateArg,
                                            )
                                        });
                                    items.extend(new_items);

                                    let num_fields_to_complete = class
                                        .record_body()
                                        .and_then(|record_body| record_body.body())
                                        .and_then(|body| {
                                            body.items()
                                                .map(|item| item.syntax().text_range())
                                                .position(|range| range.contains_inclusive(pos))
                                        })
                                        .unwrap_or(usize::MAX);
                                    let new_items = record
                                        .fields()
                                        .into_iter()
                                        .filter_map(|symbol_id| doc.symbol_map().symbol(symbol_id))
                                        .filter_map(|symbol| symbol.as_field())
                                        .take(num_fields_to_complete)
                                        .map(|field| {
                                            CompletionItem::new(
                                                field.name(),
                                                field.r#type().to_string(),
                                                CompletionItemKind::Field,
                                            )
                                        });
                                    items.extend(new_items);

                                    let new_items = doc
                                        .symbol_map()
                                        .get_all_parent_fields(class_symbol)
                                        .into_iter()
                                        .filter_map(|symbol_id| doc.symbol_map().symbol(symbol_id))
                                        .filter_map(|symbol| symbol.as_field())
                                        .map(|field| {
                                            CompletionItem::new(
                                                field.name(),
                                                field.r#type().to_string(),
                                                CompletionItemKind::Field,
                                            )
                                        });
                                    items.extend(new_items);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Some(items)
}
