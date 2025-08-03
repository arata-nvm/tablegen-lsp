use syntax::{
    SyntaxNode,
    ast::{self, AstNode},
};

use crate::symbol_map::class::ClassId;
use crate::{
    file_system::{FilePosition, SourceUnitId},
    index::IndexDatabase,
    symbol_map::SymbolMap,
};

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct CompletionItem {
    pub label: String,
    pub insert_text_snippet: Option<String>,
    pub detail: String,
    pub kind: CompletionItemKind,
}

impl CompletionItem {
    fn new_simple(
        label: impl Into<String>,
        detail: impl Into<String>,
        kind: CompletionItemKind,
    ) -> Self {
        Self {
            label: label.into(),
            insert_text_snippet: None,
            detail: detail.into(),
            kind,
        }
    }

    fn new_snippet(
        label: impl Into<String>,
        insert_text_snippet: impl Into<String>,
        detail: impl Into<String>,
        kind: CompletionItemKind,
    ) -> Self {
        Self {
            label: label.into(),
            insert_text_snippet: Some(insert_text_snippet.into()),
            detail: detail.into(),
            kind,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompletionItemKind {
    Keyword,
    Type,
    Class,
    TemplateArgument,
    Def,
    Defset,
}

pub fn exec(
    db: &dyn IndexDatabase,
    source_unit_id: SourceUnitId,
    pos: FilePosition,
    trigger_char: Option<String>,
) -> Option<Vec<CompletionItem>> {
    let parse = db.parse(pos.file);
    let index = db.index(source_unit_id);
    let symbol_map = index.symbol_map();

    let root_node = parse.syntax_node();
    let token_at_pos = root_node.token_at_offset(pos.position).left_biased()?;
    let node_at_pos = token_at_pos.parent()?;

    let mut ctx = CompletionContext::new();
    if trigger_char == Some("!".into()) {
        ctx.complete_bang_operators();
    }

    if within::<ast::StatementList>(&node_at_pos, 2).is_some() {
        ctx.complete_toplevel_keywords();
    }
    if within::<ast::InnerValue>(&node_at_pos, 2).is_some() {
        ctx.complete_primitive_values();
        ctx.complete_defs(symbol_map);
        ctx.complete_defsets(symbol_map);
        if let Some(class) = find::<ast::Class>(&node_at_pos) {
            (|| -> Option<()> {
                let class_name = class.name()?;
                let class_name = class_name.value()?;
                let class_id = symbol_map.find_class(&class_name)?;
                ctx.complete_template_arguments(symbol_map, class_id);
                None
            })();
        }
    }
    if within::<ast::ClassRef>(&node_at_pos, 2).is_some()
        || within::<ast::ClassId>(&node_at_pos, 2).is_some()
    {
        ctx.complete_classes(symbol_map);
    }
    if within::<ast::Type>(&node_at_pos, 2).is_some() {
        ctx.complete_primitive_types();
    }

    Some(ctx.finish())
}

fn find<N: AstNode<Language = syntax::Language>>(node: &SyntaxNode) -> Option<N> {
    node.ancestors().find_map(N::cast)
}

fn within<N: AstNode<Language = syntax::Language>>(
    node: &SyntaxNode,
    max_depth: usize,
) -> Option<N> {
    node.ancestors().take(max_depth).find_map(N::cast)
}

struct CompletionContext {
    items: Vec<CompletionItem>,
}

impl CompletionContext {
    fn new() -> Self {
        Self { items: Vec::new() }
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

        for &keyword in &TOPLEVEL_KEYWORDS {
            self.items.push(CompletionItem::new_simple(
                keyword,
                "",
                CompletionItemKind::Keyword,
            ));
        }
    }

    fn complete_primitive_types(&mut self) {
        const PRIMITIVE_TYPES: [&str; 5] = ["bit", "code", "dag", "int", "string"];
        for &ty in &PRIMITIVE_TYPES {
            self.items
                .push(CompletionItem::new_simple(ty, "", CompletionItemKind::Type));
        }

        self.items.push(CompletionItem::new_snippet(
            "bits",
            "bits<$1> $0",
            "",
            CompletionItemKind::Type,
        ));

        self.items.push(CompletionItem::new_snippet(
            "list",
            "list<$1> $0",
            "",
            CompletionItemKind::Type,
        ));
    }

    fn complete_primitive_values(&mut self) {
        const BOOLEAN_VALUES: [&str; 2] = ["false", "true"];
        for &value in &BOOLEAN_VALUES {
            self.items.push(CompletionItem::new_simple(
                value,
                "",
                CompletionItemKind::Keyword,
            ));
        }
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
        for &op in &BANG_OPERATORS {
            self.items.push(CompletionItem::new_simple(
                op,
                "",
                CompletionItemKind::Keyword,
            ));
        }
    }

    fn complete_classes(&mut self, symbol_map: &SymbolMap) {
        for class_id in symbol_map.iter_class() {
            let class = symbol_map.class(class_id);
            let arg_snippet = class
                .iter_template_arg()
                .enumerate()
                .map(|(i, _)| format!("${{{}}}", i + 1))
                .collect::<Vec<_>>()
                .join(", ");
            self.items.push(CompletionItem::new_snippet(
                class.name.clone(),
                format!(
                    "{}{}$0",
                    class.name,
                    if arg_snippet.is_empty() {
                        "".to_string()
                    } else {
                        format!("<{arg_snippet}>")
                    }
                ),
                "class",
                CompletionItemKind::Class,
            ));
        }
    }

    fn complete_template_arguments(&mut self, symbol_map: &SymbolMap, class_id: ClassId) {
        let class = symbol_map.class(class_id);
        for arg_id in class.iter_template_arg() {
            let arg = symbol_map.template_arg(arg_id);
            self.items.push(CompletionItem::new_simple(
                arg.name.clone(),
                arg.typ.to_string(),
                CompletionItemKind::TemplateArgument,
            ));
        }
    }

    fn complete_defs(&mut self, symbol_map: &SymbolMap) {
        for def_id in symbol_map.iter_def() {
            let def = symbol_map.def(def_id);
            self.items.push(CompletionItem::new_simple(
                def.name.clone(),
                "def",
                CompletionItemKind::Def,
            ));
        }
    }

    fn complete_defsets(&mut self, symbol_map: &SymbolMap) {
        for defset_id in symbol_map.iter_defset() {
            let defset = symbol_map.defset(defset_id);
            self.items.push(CompletionItem::new_simple(
                defset.name.clone(),
                defset.typ.to_string(),
                CompletionItemKind::Defset,
            ));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tests;

    use super::CompletionItem;

    fn check(s: &str) -> Vec<CompletionItem> {
        let (db, f) = tests::single_file(s);
        let mut result =
            super::exec(&db, f.source_unit_id(), f.marker(0), None).expect("completion failed");
        result.sort_by(|a, b| a.label.cmp(&b.label));
        result
    }

    fn check_trigger(s: &str, trigger_char: impl Into<String>) -> Vec<CompletionItem> {
        let (db, f) = tests::single_file(s);
        let mut result = super::exec(
            &db,
            f.source_unit_id(),
            f.marker(0),
            Some(trigger_char.into()),
        )
        .expect("completion failed");
        result.sort_by(|a, b| a.label.cmp(&b.label));
        result
    }

    #[test]
    fn keyword() {
        insta::assert_debug_snapshot!(check("c$"));
    }

    #[test]
    fn r#type() {
        insta::assert_debug_snapshot!(check("class Foo<i$"));
        insta::assert_debug_snapshot!(check("class Foo<int a>; class Bar : Foo$;"));
    }

    #[test]
    fn value() {
        insta::assert_debug_snapshot!(check("class Foo<int a = t$"));
        insta::assert_debug_snapshot!(check("def foo; defvar tmp = f$"));
        insta::assert_debug_snapshot!(check(
            "class Foo; defset list<Foo> foo = {} defvar tmp = f$"
        ));
    }

    #[test]
    fn bang_operator() {
        insta::assert_debug_snapshot!(check_trigger("!$", "!"));
    }
}
