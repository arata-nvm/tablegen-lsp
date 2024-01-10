use tablegen_parser::ast;
use tablegen_parser::ast::{AstNode, BodyItem};
use tablegen_parser::language::SyntaxNode;
use tablegen_parser::parser::TextSize;
use tablegen_parser::syntax_kind::SyntaxKind;

use crate::document::Document;
use crate::symbol::{Record, RecordKind, Symbol, VariableKind};

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
    let mut ctx = CompletionContext::new(doc, pos);

    let root = doc.root();
    let cur_token = root.token_at_offset(pos).left_biased()?;
    let parent_node = cur_token.parent()?;
    let parent_parent_node = parent_node.parent()?;

    if cur_token.kind() == SyntaxKind::Error && cur_token.text() == "!" {
        ctx.complete_bang_operators();
    }

    match parent_parent_node.kind() {
        SyntaxKind::StatementList => {
            ctx.complete_toplevel_keywords();
        }
        SyntaxKind::Value if parent_node.kind() == SyntaxKind::InnerValue => {
            ctx.complete_primitive_values();
            ctx.complete_defs();
            ctx.complete_values_of_class(parent_node);
        }
        SyntaxKind::ClassRef => {
            ctx.complete_classes();
        }
        SyntaxKind::FieldLet => {
            ctx.complete_values_of_parent_class(parent_node);
        }
        _ if ast::Type::can_cast(parent_parent_node.kind()) => {
            ctx.complete_primitive_types();
            ctx.complete_classes();
            ctx.complete_let_keyword(parent_node);
        }
        _ => {}
    }

    Some(ctx.finish())
}

struct CompletionContext<'a> {
    doc: &'a Document,
    pos: TextSize,
    items: Vec<CompletionItem>,
}

impl<'a> CompletionContext<'a> {
    fn new(doc: &'a Document, pos: TextSize) -> Self {
        Self {
            doc,
            pos,
            items: Vec::new(),
        }
    }

    fn finish(self) -> Vec<CompletionItem> {
        self.items
    }

    fn complete_toplevel_keywords(&mut self) {
        const TOPLEVEL_KEYWORDS: [&str; 12] = [
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

        self.add_items(&TOPLEVEL_KEYWORDS, CompletionItemKind::Keyword);
    }

    fn complete_primitive_types(&mut self) {
        const PRIMITIVE_TYPES: [&str; 7] = ["bit", "bits", "code", "dag", "int", "list", "string"];
        self.add_items(&PRIMITIVE_TYPES, CompletionItemKind::Type);
    }

    fn complete_primitive_values(&mut self) {
        const BOOLEAN_VALUES: [&str; 2] = ["false", "true"];
        self.add_items(&BOOLEAN_VALUES, CompletionItemKind::Keyword);
    }

    fn complete_bang_operators(&mut self) {
        const BANG_OPERATORS: [&str; 48] = [
            "concat",
            "add",
            "sub",
            "mul",
            "div",
            "not",
            "log2",
            "and",
            "or",
            "xor",
            "sra",
            "srl",
            "shl",
            "listconcat",
            "listsplat",
            "strconcat",
            "interleave",
            "substr",
            "find",
            "cast",
            "subst",
            "foreach",
            "filter",
            "foldl",
            "head",
            "tail",
            "size",
            "empty",
            "if",
            "eq",
            "isa",
            "dag",
            "ne",
            "le",
            "lt",
            "ge",
            "gt",
            "setdagop",
            "getdagop",
            "exists",
            "listremove",
            "tolower",
            "toupper",
            "range",
            "getdagarg",
            "getdagname",
            "setdagarg",
            "setdagname",
        ];
        self.add_items(&BANG_OPERATORS, CompletionItemKind::Keyword);
    }

    fn complete_classes(&mut self) {
        let symbol_map = self.doc.symbol_map();
        for symbol in symbol_map.global_symbols() {
            // NOTE: symbolが外部のファイルで定義されていた場合、バグる可能性がある
            let symbol_pos = symbol.define_loc().1.start();
            if symbol_pos >= self.pos {
                continue;
            }

            let Symbol::Record(record) = symbol else {
                continue;
            };
            if matches!(record.kind(), RecordKind::Class) {
                self.add_item(record.name(), "class", CompletionItemKind::Class)
            }
        }
    }

    fn complete_defs(&mut self) {
        let symbol_map = self.doc.symbol_map();
        for symbol in symbol_map.global_symbols() {
            // NOTE: symbolが外部のファイルで定義されていた場合、バグる可能性がある
            let symbol_pos = symbol.define_loc().1.start();
            if symbol_pos >= self.pos {
                continue;
            }

            match symbol {
                Symbol::Record(record) => {
                    if let RecordKind::Def = record.kind() {
                        self.add_item(record.name(), "def", CompletionItemKind::Def)
                    }
                }
                Symbol::RecordField(_) => {}
                Symbol::Variable(variable) => match variable.kind() {
                    VariableKind::Defset => self.add_item(
                        variable.name(),
                        variable.r#type().to_string(),
                        CompletionItemKind::Defset,
                    ),
                    VariableKind::Defvar => self.add_item(
                        variable.name(),
                        variable.r#type().to_string(),
                        CompletionItemKind::Defvar,
                    ),
                    VariableKind::Temporary => {}
                },
            }
        }
    }

    fn complete_values_of_class(&mut self, parent_node: SyntaxNode) -> Option<()> {
        let symbol_map = self.doc.symbol_map();

        let class = parent_node
            .ancestors()
            .find(|node| node.kind() == SyntaxKind::Class)
            .and_then(ast::Class::cast)?;
        let class_name_range = class.name()?.range()?;
        let class_symbol = symbol_map.get_symbol_at(class_name_range.start())?;
        let record = class_symbol.as_record()?;

        self.complete_template_args_of_class(&class, record);

        let body = class.record_body()?.body()?;
        if body.syntax().text_range().contains_inclusive(self.pos) {
            self.complete_fields_of_class(&body, record);
            self.complete_fields_of_parent_class(class_symbol);
        }

        Some(())
    }

    fn complete_values_of_parent_class(&mut self, parent_node: SyntaxNode) -> Option<()> {
        let symbol_map = self.doc.symbol_map();

        let class = parent_node
            .ancestors()
            .find(|node| node.kind() == SyntaxKind::Class)
            .and_then(ast::Class::cast)?;
        let class_name_range = class.name()?.range()?;
        let class_symbol = symbol_map.get_symbol_at(class_name_range.start())?;

        let body = class.record_body()?.body()?;
        if body.syntax().text_range().contains_inclusive(self.pos) {
            self.complete_fields_of_parent_class(class_symbol);
        }

        Some(())
    }

    fn complete_template_args_of_class(
        &mut self,
        class: &ast::Class,
        record: &Record,
    ) -> Option<()> {
        let symbol_map = self.doc.symbol_map();

        let args = class.template_arg_list()?.args();
        for arg in args {
            let arg_pos = arg.syntax().text_range().end();
            if arg_pos >= self.pos {
                break;
            }

            let field = arg
                .name()
                .and_then(|name| name.value())
                .and_then(|name| record.find_template_arg(&name))
                .and_then(|symbol_id| symbol_map.symbol(*symbol_id))
                .and_then(|symbol| symbol.as_field());
            if let Some(field) = field {
                self.add_item(
                    field.name(),
                    field.r#type().to_string(),
                    CompletionItemKind::TemplateArg,
                );
            }
        }

        Some(())
    }

    fn complete_fields_of_class(&mut self, body: &ast::Body, record: &Record) -> Option<()> {
        let symbol_map = self.doc.symbol_map();

        for item in body.items() {
            let item_pos = item.syntax().text_range().end();
            if item_pos >= self.pos {
                break;
            }

            if let BodyItem::FieldDef(field_def) = item {
                let field = field_def
                    .name()
                    .and_then(|name| name.value())
                    .and_then(|name| record.find_field(&name))
                    .and_then(|symbol_id| symbol_map.symbol(*symbol_id))
                    .and_then(|symbol| symbol.as_field());
                if let Some(field) = field {
                    self.add_item(
                        field.name(),
                        field.r#type().to_string(),
                        CompletionItemKind::Field,
                    );
                }
            }
        }

        Some(())
    }

    fn complete_fields_of_parent_class(&mut self, class_symbol: &Symbol) {
        let symbol_map = self.doc.symbol_map();

        let parent_fields = symbol_map.get_all_parent_fields(class_symbol);
        for symbol_id in parent_fields {
            let field = symbol_map
                .symbol(symbol_id)
                .and_then(|symbol| symbol.as_field());
            if let Some(field) = field {
                self.add_item(
                    field.name(),
                    field.r#type().to_string(),
                    CompletionItemKind::Field,
                );
            }
        }
    }

    fn complete_let_keyword(&mut self, parent_node: SyntaxNode) {
        let is_in_field_def = parent_node
            .ancestors()
            .find(|node| node.kind() == SyntaxKind::FieldDef)
            .and_then(ast::FieldDef::cast)
            .is_some();
        if is_in_field_def {
            self.add_item("let", "", CompletionItemKind::Keyword);
        }
    }

    fn add_item(
        &mut self,
        label: impl Into<String>,
        detail: impl Into<String>,
        kind: CompletionItemKind,
    ) {
        self.items.push(CompletionItem::new(label, detail, kind));
    }

    fn add_items(&mut self, labels: &[&str], kind: CompletionItemKind) {
        for &label in labels {
            self.items.push(CompletionItem::new(label, "", kind));
        }
    }
}
