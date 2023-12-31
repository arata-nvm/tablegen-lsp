use std::fs;
use std::path::PathBuf;

use async_lsp::LanguageClient;
use lsp_types::{PublishDiagnosticsParams, Url};

use tablegen_analyzer::document::Document;
use tablegen_analyzer::source::{Dependencies, Source, SourceSet};
use tablegen_parser::ast::AstNode;
use tablegen_parser::language::SyntaxNode;
use tablegen_parser::{ast, grammar};

use crate::compat::analyzer2lsp;
use crate::server::TableGenLanguageServer;

impl TableGenLanguageServer {
    pub(super) fn check_file(&mut self, uri: Url, version: i32, text: String) {
        let source_set = self.to_source_set(uri.clone(), text);
        let mut document = Document::parse_set(source_set);
        self.publish_errors(uri, version, &mut document);
        self.document_map.update_document(document.id(), document);
    }

    fn publish_errors(&mut self, uri: Url, version: i32, document: &mut Document) {
        let diags = document
            .take_errors()
            .into_iter()
            .map(|error| analyzer2lsp::error(&document, error))
            .collect();
        self.client
            .publish_diagnostics(PublishDiagnosticsParams::new(uri, diags, Some(version)))
            .unwrap();
    }

    fn to_source_set(&mut self, uri: Url, text: String) -> SourceSet {
        let doc_id = self.document_map.assign_document_id(uri.clone());
        let path = PathBuf::from(uri.path());
        let mut dependencies = Dependencies::new();
        self.list_dependencies(path, &text, &mut dependencies);
        SourceSet::new(Source::new(doc_id, text), dependencies)
    }

    fn list_dependencies(&mut self, path: PathBuf, text: &str, dependencies: &mut Dependencies) {
        let (root, _) = grammar::parse(text);
        let include_paths = self.extract_include_paths(root).unwrap_or(vec![]);
        for include_path in include_paths {
            let Some(resolved_path) = self.resolve_path(path.clone(), &include_path) else {
                continue;
            };
            let Ok(doc_uri) = Url::from_file_path(&resolved_path) else {
                continue;
            };
            let Ok(doc_text) = fs::read_to_string(resolved_path) else {
                continue;
            };
            let doc_id = self.document_map.assign_document_id(doc_uri);
            dependencies.insert(include_path, Source::new(doc_id, doc_text));
        }
    }

    fn extract_include_paths(&mut self, root: SyntaxNode) -> Option<Vec<String>> {
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

    fn resolve_path(&mut self, base_path: PathBuf, include_path: &str) -> Option<PathBuf> {
        let parent_dir = base_path.parent()?;
        let resolved_path = parent_dir.join(include_path);
        if resolved_path.exists() {
            Some(resolved_path)
        } else {
            None
        }
    }
}
