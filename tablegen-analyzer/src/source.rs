use std::collections::HashMap;

use crate::document::DocumentId;

#[derive(Debug)]
pub struct SourceSet {
    pub root: Source,
    pub dependencies: Dependencies,
}

pub type Dependencies = HashMap<String, Source>;

impl SourceSet {
    pub fn new(root: Source, dependencies: Dependencies) -> Self {
        Self { root, dependencies }
    }
}

#[derive(Debug)]
pub struct Source {
    pub document_id: DocumentId,
    pub text: String,
}

impl Source {
    pub fn new(document_id: DocumentId, text: String) -> Self {
        Self { document_id, text }
    }
}
