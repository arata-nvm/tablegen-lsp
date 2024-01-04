use std::collections::HashMap;

use tablegen_parser::language::SyntaxNode;

use crate::document::DocumentId;

#[derive(Debug)]
pub struct SourceSet {
    pub root: SourceWithText,
    pub dependencies: Dependencies,
}

pub type Dependencies = HashMap<String, Source>;

impl SourceSet {
    pub fn new(root: SourceWithText, dependencies: Dependencies) -> Self {
        Self { root, dependencies }
    }
}

#[derive(Debug)]
pub struct Source {
    pub document_id: DocumentId,
    pub root_node: SyntaxNode,
}

impl Source {
    pub fn new(document_id: DocumentId, root_node: SyntaxNode) -> Self {
        Self {
            document_id,
            root_node,
        }
    }
}

#[derive(Debug)]
pub struct SourceWithText {
    pub document_id: DocumentId,
    pub text: String,
    pub root_node: SyntaxNode,
}

impl SourceWithText {
    pub fn new(document_id: DocumentId, text: String, root_node: SyntaxNode) -> Self {
        Self {
            document_id,
            text,
            root_node,
        }
    }
}
