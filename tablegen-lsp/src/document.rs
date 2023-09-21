pub mod index;

use ropey::Rope;
use tablegen_parser::{
    error::{Span, SyntaxError},
    grammar,
};
use tower_lsp::lsp_types::{Diagnostic, Position, Range, Url};

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

    fn add_syntax_errors(&mut self, errors: Vec<&SyntaxError>) {
        for error in errors {
            let range = span_to_range(&self.text, error.span.clone());
            let diagnostic = Diagnostic::new_simple(range, error.message.to_string());
            self.diagnostics.push(diagnostic);
        }
    }
}
