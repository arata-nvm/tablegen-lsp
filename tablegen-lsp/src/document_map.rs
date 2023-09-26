use std::collections::HashMap;

use tablegen_analyzer::document::{Document, DocumentId};
use tower_lsp::lsp_types::Url;

pub struct DocumentMap {
    uri_to_id: HashMap<Url, DocumentId>,
    id_to_uri: Vec<Url>,
    document_map: HashMap<DocumentId, Document>,
}

impl DocumentMap {
    pub fn new() -> Self {
        Self {
            uri_to_id: HashMap::new(),
            id_to_uri: Vec::new(),
            document_map: HashMap::new(),
        }
    }

    pub fn assign_document_id(&mut self, uri: Url) -> DocumentId {
        if let Some(doc_id) = self.uri_to_id.get(&uri) {
            return *doc_id;
        }

        let doc_id = self.next_document_id(uri.clone());
        self.uri_to_id.insert(uri, doc_id);
        doc_id
    }

    pub fn to_uri(&self, doc_id: DocumentId) -> Option<&Url> {
        self.id_to_uri.get::<usize>(doc_id.into())
    }

    pub fn to_document_id(&self, uri: &Url) -> Option<&DocumentId> {
        self.uri_to_id.get(uri)
    }

    pub fn update_document(&mut self, doc_id: DocumentId, document: Document) {
        self.document_map.insert(doc_id, document);
    }

    pub fn find_document(&self, doc_id: &DocumentId) -> Option<&Document> {
        self.document_map.get(doc_id)
    }

    fn next_document_id(&mut self, uri: Url) -> DocumentId {
        let doc_id = self.id_to_uri.len();
        self.id_to_uri.push(uri);
        DocumentId::new(doc_id)
    }
}
