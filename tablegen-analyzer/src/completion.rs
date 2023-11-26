use tablegen_parser::parser::TextSize;

pub struct CompletionItem {
    pub label: String,
    pub detail: String,
    pub kind: CompletionItemKind,
}

pub enum CompletionItemKind {
    Keyword,
    Type,
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
    complete_type(&mut items);
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
        items.push(CompletionItem::new(keyword, "", CompletionItemKind::Keyword));
    }
}

fn complete_type(items: &mut Vec<CompletionItem>) {
    const TYPES: [&str; 7] = [
        "bit",
        "bits",
        "code",
        "dag",
        "int",
        "list",
        "string",
    ];

    for typ in TYPES {
        items.push(CompletionItem::new(typ, "", CompletionItemKind::Type))
    }
}
