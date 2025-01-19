use async_lsp::lsp_types;
use ide::{
    file_system::{FileRange, FileSystem},
    handlers::{
        completion::{CompletionItem, CompletionItemKind},
        document_link::DocumentLink,
        document_symbol::DocumentSymbolKind,
        folding_range::FoldingRange,
        hover::Hover,
        inlay_hint::{InlayHint, InlayHintKind},
    },
};
use text_size::{TextRange, TextSize};

use ide::handlers::diagnostics::Diagnostic;
use ide::handlers::document_symbol::DocumentSymbol;
use ide::line_index::LineIndex;

use crate::vfs::{UrlExt, Vfs};

pub fn position(line_index: &LineIndex, position: TextSize) -> lsp_types::Position {
    let line = line_index.pos_to_line(position);
    let line_first = line_index.line_to_pos(line);
    let character = position - line_first;
    lsp_types::Position::new(
        line.try_into().expect("line out of range"),
        character.into(),
    )
}

pub fn range(line_index: &LineIndex, range: TextRange) -> lsp_types::Range {
    lsp_types::Range::new(
        position(line_index, range.start()),
        position(line_index, range.end()),
    )
}

pub fn location(vfs: &Vfs, line_index: &LineIndex, file_range: FileRange) -> lsp_types::Location {
    let path = vfs.path_for_file(&file_range.file);
    lsp_types::Location::new(
        UrlExt::from_file_path(path),
        range(line_index, file_range.range),
    )
}

pub fn diagnostic(line_index: &LineIndex, diag: Diagnostic) -> lsp_types::Diagnostic {
    lsp_types::Diagnostic::new_simple(range(line_index, diag.location.range), diag.message)
}

#[allow(deprecated)]
pub fn document_symbol(
    line_index: &LineIndex,
    symbol: DocumentSymbol,
) -> lsp_types::DocumentSymbol {
    let range = range(line_index, symbol.range);
    let children = match symbol.children.is_empty() {
        true => None,
        false => Some(
            symbol
                .children
                .into_iter()
                .map(|it| document_symbol(line_index, it))
                .collect(),
        ),
    };
    lsp_types::DocumentSymbol {
        name: symbol.name.to_string(),
        detail: Some(symbol.typ.to_string()),
        kind: match symbol.kind {
            DocumentSymbolKind::Class => lsp_types::SymbolKind::CLASS,
            DocumentSymbolKind::TemplateArgument => lsp_types::SymbolKind::PROPERTY,
            DocumentSymbolKind::Field => lsp_types::SymbolKind::FIELD,
            DocumentSymbolKind::Def => lsp_types::SymbolKind::VARIABLE,
            DocumentSymbolKind::Variable => lsp_types::SymbolKind::VARIABLE,
            DocumentSymbolKind::Defset => lsp_types::SymbolKind::VARIABLE,
            DocumentSymbolKind::Multiclass => lsp_types::SymbolKind::CLASS,
        },
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children,
    }
}

pub fn hover(hover: Hover) -> lsp_types::Hover {
    let mut contents = vec![lsp_types::MarkedString::from_language_code(
        String::from("tablegen"),
        hover.signature,
    )];
    if let Some(document) = hover.document {
        contents.push(lsp_types::MarkedString::from_markdown(String::from("***")));
        contents.push(lsp_types::MarkedString::from_markdown(document));
    }

    lsp_types::Hover {
        contents: lsp_types::HoverContents::Array(contents),
        range: None,
    }
}

pub fn inlay_hint(line_index: &LineIndex, inlay_hint: InlayHint) -> lsp_types::InlayHint {
    lsp_types::InlayHint {
        position: position(line_index, inlay_hint.position),
        label: lsp_types::InlayHintLabel::String(inlay_hint.label),
        kind: None,
        text_edits: None,
        tooltip: None,
        padding_left: match inlay_hint.kind {
            InlayHintKind::TemplateArg => Some(false),
            InlayHintKind::FieldLet => Some(true),
        },
        padding_right: match inlay_hint.kind {
            InlayHintKind::TemplateArg => Some(true),
            InlayHintKind::FieldLet => Some(false),
        },
        data: None,
    }
}

pub fn completion_item(item: CompletionItem) -> lsp_types::CompletionItem {
    let mut lsp_item = lsp_types::CompletionItem::new_simple(item.label, item.detail);
    lsp_item.kind = Some(match item.kind {
        CompletionItemKind::Keyword => lsp_types::CompletionItemKind::KEYWORD,
        CompletionItemKind::Type => lsp_types::CompletionItemKind::CLASS,
        CompletionItemKind::Class => lsp_types::CompletionItemKind::CLASS,
    });
    if let Some(insert_text_snippet) = item.insert_text_snippet {
        lsp_item.insert_text_format = Some(lsp_types::InsertTextFormat::SNIPPET);
        lsp_item.insert_text = Some(insert_text_snippet);
    }
    lsp_item
}

pub fn document_link(
    vfs: &Vfs,
    line_index: &LineIndex,
    link: DocumentLink,
) -> lsp_types::DocumentLink {
    lsp_types::DocumentLink {
        range: range(line_index, link.range),
        target: Some(UrlExt::from_file_path(vfs.path_for_file(&link.target))),
        tooltip: None,
        data: None,
    }
}

pub fn folding_range(line_index: &LineIndex, range: FoldingRange) -> lsp_types::FoldingRange {
    lsp_types::FoldingRange {
        start_line: line_index
            .pos_to_line(range.range.start())
            .try_into()
            .unwrap(),
        start_character: None,
        end_line: line_index
            .pos_to_line(range.range.end())
            .try_into()
            .unwrap(),
        end_character: None,
        kind: Some(lsp_types::FoldingRangeKind::Region),
        collapsed_text: None,
    }
}
