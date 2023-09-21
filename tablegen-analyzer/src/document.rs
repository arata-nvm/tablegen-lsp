pub mod compat;
pub mod index;
pub mod symbol;

use ropey::Rope;
use tablegen_parser::{error::SyntaxError, grammar};
use tower_lsp::lsp_types::{Diagnostic, Location, Position, Url};

use self::{
    compat::{position_to_loc, source_location_to_location, span_to_range},
    index::TableGenDocumentIndex,
};

#[derive(Debug)]
pub struct TableGenDocument {
    pub uri: Url,
    pub text: Rope,
    pub diagnostics: Vec<Diagnostic>,
    pub index: TableGenDocumentIndex,
}

impl TableGenDocument {
    fn new(uri: Url, text: &str, index: TableGenDocumentIndex) -> Self {
        Self {
            uri,
            text: Rope::from_str(text),
            diagnostics: Vec::new(),
            index,
        }
    }

    pub fn parse(uri: Url, text: String) -> Self {
        let file = grammar::parse(&text);
        let index = TableGenDocumentIndex::create_index(uri.clone(), &file);
        let mut document = Self::new(uri, &text, index);
        document.add_syntax_errors(file.errors());
        document
    }

    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }

    pub fn get_definition(&self, position: Position) -> Option<Location> {
        let loc = position_to_loc(&self.text, position);
        let Some(symbol) = self.index.get_symbol_at(loc) else { return None; };
        Some(source_location_to_location(
            &self.text,
            symbol.define_loc.clone(),
        ))
    }

    fn add_syntax_errors(&mut self, errors: Vec<&SyntaxError>) {
        for error in errors {
            let range = span_to_range(&self.text, error.span.clone());
            let diagnostic = Diagnostic::new_simple(range, error.message.to_string());
            self.diagnostics.push(diagnostic);
        }
    }
}
