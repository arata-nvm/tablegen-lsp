use syntax::syntax_kind::SyntaxKind;

use crate::{eval::EvalDatabase, file_system::FilePosition};

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct CompletionItem {
    pub label: String,
    pub detail: String,
    pub kind: CompletionItemKind,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompletionItemKind {
    Keyword,
}

pub fn exec(db: &dyn EvalDatabase, pos: FilePosition) -> Option<Vec<CompletionItem>> {
    let parse = db.parse(pos.file);
    let root_node = parse.syntax_node();
    let cur_token = root_node.token_at_offset(pos.position).left_biased()?;
    let parent_node = cur_token.parent()?;
    let parent_parent_node = parent_node.parent()?;

    let mut ctx = CompletionContext::new();
    match parent_parent_node.kind() {
        SyntaxKind::StatementList => ctx.complete_toplevel_keywords(),
        _ => {}
    }

    Some(ctx.finish())
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
}

#[cfg(test)]
mod tests {
    use crate::tests;

    use super::CompletionItem;

    fn check(s: &str) -> Vec<CompletionItem> {
        let (db, f) = tests::single_file(s);
        super::exec(&db, f.marker(0)).expect("completion failed")
    }

    #[test]
    fn keyword() {
        insta::assert_debug_snapshot!(check("c$"));
    }
}
