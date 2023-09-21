use tablegen_parser::node::SyntaxNode;
use tower_lsp::lsp_types::Url;

#[derive(Debug)]
pub struct TableGenDocumentIndex {
    uri: Url,
}

impl TableGenDocumentIndex {
    fn new(uri: Url) -> Self {
        Self { uri }
    }

    pub fn create_index(uri: Url, _file: &SyntaxNode) -> Self {
        Self::new(uri)
        // TODO
    }
}
