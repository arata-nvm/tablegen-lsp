pub mod index;

use ropey::Rope;
use tablegen_parser::{
    error::{Span, SyntaxError},
    grammar,
};
use tower_lsp::lsp_types::{Diagnostic, Position, Range, Url};

use self::index::TableGenDocumentIndex;

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
            let range = self.span_to_range(&error.span);
            let diagnostic = Diagnostic::new_simple(range, error.message.to_string());
            self.diagnostics.push(diagnostic);
        }
    }

    fn span_to_range(&self, span: &Span) -> Range {
        let start = self.loc_to_position(span.start);
        let end = self.loc_to_position(span.end);
        Range::new(start, end)
    }

    fn loc_to_position(&self, loc: usize) -> Position {
        let line = self.text.try_char_to_line(loc).unwrap_or_default();
        let line_first = self.text.try_line_to_char(line).unwrap_or_default();
        let character = loc - line_first;
        Position::new(line as u32, character as u32)
    }
}
