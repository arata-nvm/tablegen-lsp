use std::fs;
use std::path::{Path, PathBuf};

use tablegen_parser::ast::AstNode;
use tablegen_parser::error::TableGenError;
use tablegen_parser::language::SyntaxNode;
use tablegen_parser::{ast, grammar};

use crate::config::Config;
use crate::document::Document;
use crate::document_map::{DocumentMap, DocumentPath};
use crate::source::{Dependencies, Source, SourceSet, SourceWithText};

pub struct TableGenLanguageServerImpl<P: DocumentPath> {
    document_map: DocumentMap<P>,
    config: Config,
}

impl<P: DocumentPath> TableGenLanguageServerImpl<P> {
    pub fn new() -> Self {
        Self {
            document_map: DocumentMap::new(),
            config: Default::default(),
        }
    }

    pub fn set_include_path(&mut self, include_path: Vec<PathBuf>) {
        self.config.include_path = include_path;
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

    pub fn check_file(&mut self, path: P, text: String) -> Vec<TableGenError> {
        let source_set = self.convert_source_to_source_set(path.clone(), text);
        let mut document = Document::parse_set(source_set);
        let errors = document.take_errors();
        self.document_map.update_document(document.id(), document);
        errors
    }

    fn convert_source_to_source_set(&mut self, path: P, text: String) -> SourceSet {
        let doc_id = self.document_map.assign_document_id(path.clone());
        let (root_node, _) = grammar::parse(&text);

        let mut dependencies = Dependencies::new();
        self.list_dependencies(path.to_path(), root_node.clone(), &mut dependencies);

        SourceSet::new(SourceWithText::new(doc_id, text, root_node), dependencies)
    }

    fn list_dependencies(
        &mut self,
        path: &Path,
        root: SyntaxNode,
        dependencies: &mut Dependencies,
    ) {
        let include_paths = extract_include_paths(root.clone()).unwrap_or_default();
        for include_path in include_paths {
            let Some(resolved_path) = self.resolve_path(path, &include_path) else {
                continue;
            };

            let Some(doc_uri) = P::from_path(&resolved_path) else {
                continue;
            };
            let doc_id = self.document_map.assign_document_id(doc_uri);

            let Ok(doc_text) = fs::read_to_string(&resolved_path) else {
                continue;
            };
            let (doc_root, _) = grammar::parse(&doc_text);

            dependencies.insert(include_path, Source::new(doc_id, doc_root.clone()));
            self.list_dependencies(&resolved_path, doc_root, dependencies);
        }
    }

    fn resolve_path<BasePath: AsRef<Path>>(
        &self,
        base_path: BasePath,
        include_path: &str,
    ) -> Option<PathBuf> {
        let parent_dir = base_path.as_ref().parent()?;
        let include_dirs = [parent_dir]
            .into_iter()
            .chain(self.config.include_path.iter().map(|it| it.as_path()));

        for include_dir in include_dirs {
            let resolved_path = include_dir.join(include_path);
            if resolved_path.exists() {
                return Some(resolved_path);
            }
        }
        None
    }
}

fn extract_include_paths(root: SyntaxNode) -> Option<Vec<String>> {
    let root = ast::Root::cast(root)?;
    let statement_list = root.statement_list()?;
    let include_paths = statement_list
        .statements()
        .filter_map(|stmt| match stmt {
            ast::Statement::Include(include) => Some(include),
            _ => None,
        })
        .filter_map(|include| include.path())
        .map(|path| path.value())
        .collect();
    Some(include_paths)
}
