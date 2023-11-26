use tablegen_parser::parser::TextSize;

pub struct CompletionItem {
    pub label: String,
    pub detail: String,
    pub kind: CompletionItemKind,
}

pub enum CompletionItemKind {
    Keyword,
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

pub fn completion(_pos: TextSize) -> Option<Vec<CompletionItem>> {
    let mut items = Vec::new();
    complete_keyword(&mut items);
    Some(items)
}

fn complete_keyword(items: &mut Vec<CompletionItem>) {
    const KEYWORDS: [&str; 25] = [
        "assert",
        "bit",
        "bits",
        "class",
        "code",
        "dag",
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
        "int",
        "let",
        "list",
        "multiclass",
        "string",
        "then",
        "true",
    ];

    for keyword in KEYWORDS {
        items.push(CompletionItem::new(keyword, "", CompletionItemKind::Keyword));
    }
}