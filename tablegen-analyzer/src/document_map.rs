use std::collections::HashMap;
use std::hash::Hash;
use std::path::{Path, PathBuf};

use url::Url;

use crate::document::{Document, DocumentId};

pub trait DocumentPath: Eq + Hash + Clone {
    fn to_path(&self) -> &Path;

    fn from_path(path: &Path) -> Option<Self>;
}

impl DocumentPath for PathBuf {
    fn to_path(&self) -> &Path {
        self
    }

    fn from_path(path: &Path) -> Option<Self> {
        Some(path.into())
    }
}

impl DocumentPath for Url {
    fn to_path(&self) -> &Path {
        self.path().as_ref()
    }

    fn from_path(path: &Path) -> Option<Self> {
        Url::from_file_path(path).ok()
    }
}

pub struct DocumentMap<P: DocumentPath> {
    path_to_id: HashMap<P, DocumentId>,
    id_to_path: Vec<P>,
    document_map: HashMap<DocumentId, Document>,
}

impl<P: DocumentPath> DocumentMap<P> {
    pub fn new() -> Self {
        Self {
            path_to_id: HashMap::new(),
            id_to_path: Vec::new(),
            document_map: HashMap::new(),
        }
    }

    pub fn assign_document_id(&mut self, path: P) -> DocumentId {
        if let Some(doc_id) = self.path_to_id.get(&path) {
            return *doc_id;
        }

        let doc_id = self.next_document_id(path.clone());
        self.path_to_id.insert(path, doc_id);
        doc_id
    }

    pub fn to_path(&self, doc_id: DocumentId) -> Option<&P> {
        self.id_to_path.get::<usize>(doc_id.into())
    }

    pub fn to_document_id(&self, path: &P) -> Option<&DocumentId> {
        self.path_to_id.get(path)
    }

    pub fn update_document(&mut self, doc_id: DocumentId, document: Document) {
        self.document_map.insert(doc_id, document);
    }

    pub fn find_document(&self, doc_id: &DocumentId) -> Option<&Document> {
        self.document_map.get(doc_id)
    }

    fn next_document_id(&mut self, path: P) -> DocumentId {
        let doc_id = self.id_to_path.len();
        self.id_to_path.push(path);
        DocumentId::new(doc_id)
    }
}
