use ecow::EcoString;
use id_arena::Id;
use tablegen_parser::error::Span;
use tower_lsp::lsp_types::Url;

pub type TableGenSymbolId = Id<TableGenSymbol>;

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub uri: Url,
    pub span: Span,
}

impl SourceLocation {
    pub fn new(uri: Url, span: Span) -> Self {
        Self { uri, span }
    }
}

#[derive(Debug)]
pub struct TableGenSymbol {
    pub name: EcoString,
    pub define_loc: SourceLocation,
}

impl TableGenSymbol {
    pub fn new(name: EcoString, uri: Url, define_loc: Span) -> Self {
        Self {
            name,
            define_loc: SourceLocation::new(uri, define_loc),
        }
    }
}
