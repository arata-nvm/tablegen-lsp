use std::collections::HashMap;

use tablegen_analyzer::document::{TableGenDocument, TableGenDocumentId};
use tower_lsp::lsp_types::Url;

pub struct TableGenDocumentMap {
    uri_to_id: HashMap<Url, TableGenDocumentId>,
    id_to_uri: Vec<Url>,
    document_map: HashMap<TableGenDocumentId, TableGenDocument>,
}

impl TableGenDocumentMap {
    pub fn new() -> Self {
        Self {
            uri_to_id: HashMap::new(),
            id_to_uri: Vec::new(),
            document_map: HashMap::new(),
        }
    }

    pub fn assign_document_id(&mut self, uri: Url) -> TableGenDocumentId {
        if let Some(doc_id) = self.uri_to_id.get(&uri) {
            return *doc_id;
        }

        let doc_id = self.next_document_id(uri.clone());
        self.uri_to_id.insert(uri, doc_id);
        doc_id
    }

    pub fn to_uri(&self, doc_id: TableGenDocumentId) -> Option<&Url> {
        self.id_to_uri.get::<usize>(doc_id.into())
    }

    pub fn to_document_id(&self, uri: &Url) -> Option<&TableGenDocumentId> {
        self.uri_to_id.get(uri)
    }

    pub fn update_document(&mut self, doc_id: TableGenDocumentId, document: TableGenDocument) {
        self.document_map.insert(doc_id, document);
    }

    pub fn find_document(&self, doc_id: &TableGenDocumentId) -> Option<&TableGenDocument> {
        self.document_map.get(doc_id)
    }

    fn next_document_id(&mut self, uri: Url) -> TableGenDocumentId {
        let doc_id = self.id_to_uri.len();
        self.id_to_uri.push(uri);
        TableGenDocumentId::new(doc_id)
    }
}
