use async_lsp::lsp_types;
use ide::{
    file_system::{FileRange, FileSystem},
    handlers::{
        document_symbol::DocumentSymbolKind,
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
        },
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children,
    }
}

pub fn hover(hover: Hover) -> lsp_types::Hover {
    lsp_types::Hover {
        contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::LanguageString(
            lsp_types::LanguageString {
                language: String::from("tablegen"),
                value: hover.content,
            },
        )),
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
