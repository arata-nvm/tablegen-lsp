use std::hash::Hash;

use crate::document::Document;
use crate::document_map::DocumentMap;

pub struct TableGenLanguageServerImpl<P: Eq + Hash + Clone> {
    pub document_map: DocumentMap<P>,
}

impl<P: Eq + Hash + Clone> TableGenLanguageServerImpl<P> {
    pub fn new() -> Self {
        Self {
            document_map: DocumentMap::new(),
        }
    }

    pub fn with_document<T>(
        &self,
        path: P,
        f: impl FnOnce(&DocumentMap<P>, &Document) -> Option<T>,
    ) -> Option<T> {
        let doc_id = self.document_map.to_document_id(&path)?;
        let document = self.document_map.find_document(doc_id)?;
        f(&self.document_map, document)
    }
}
