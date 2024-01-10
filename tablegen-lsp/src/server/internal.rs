use std::fs;
use std::path::{Path, PathBuf};

use async_lsp::LanguageClient;
use lsp_types::{PublishDiagnosticsParams, Url};

use tablegen_analyzer::document::Document;
use tablegen_analyzer::source::{Dependencies, Source, SourceSet, SourceWithText};
use tablegen_parser::ast::AstNode;
use tablegen_parser::language::SyntaxNode;
use tablegen_parser::{ast, grammar};

use crate::compat::analyzer2lsp;
use crate::server::TableGenLanguageServer;

impl TableGenLanguageServer {
    pub(super) fn check_file(&mut self, uri: Url, version: i32, text: String) {
        let source_set = self.convert_source_to_source_set(uri.clone(), text);
        let mut document = Document::parse_set(source_set);
        self.publish_errors(uri, version, &mut document);
        self.impl_
            .document_map
            .update_document(document.id(), document);
    }

    fn publish_errors(&mut self, uri: Url, version: i32, document: &mut Document) {
        let diags = document
            .take_errors()
            .into_iter()
            .map(|error| analyzer2lsp::error(document, error))
            .collect();
        self.client
            .publish_diagnostics(PublishDiagnosticsParams::new(uri, diags, Some(version)))
            .unwrap();
    }

    fn convert_source_to_source_set(&mut self, uri: Url, text: String) -> SourceSet {
        let doc_id = self.impl_.document_map.assign_document_id(uri.clone());
        let (root_node, _) = grammar::parse(&text);

        let mut dependencies = Dependencies::new();
        self.list_dependencies(uri.path(), root_node.clone(), &mut dependencies);

        SourceSet::new(SourceWithText::new(doc_id, text, root_node), dependencies)
    }

    fn list_dependencies<P: AsRef<Path>>(
        &mut self,
        path: P,
        root: SyntaxNode,
        dependencies: &mut Dependencies,
    ) {
        let path = path.as_ref();
        let include_paths = extract_include_paths(root.clone()).unwrap_or_default();
        for include_path in include_paths {
            let Some(resolved_path) = resolve_path(path, &include_path) else {
                continue;
            };

            let Ok(doc_uri) = Url::from_file_path(&resolved_path) else {
                continue;
            };
            let doc_id = self.impl_.document_map.assign_document_id(doc_uri);

            let Ok(doc_text) = fs::read_to_string(&resolved_path) else {
                continue;
            };
            let (doc_root, _) = grammar::parse(&doc_text);

            dependencies.insert(include_path, Source::new(doc_id, doc_root.clone()));
            self.list_dependencies(resolved_path, doc_root, dependencies);
        }
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

fn resolve_path<P: AsRef<Path>>(base_path: P, include_path: &str) -> Option<PathBuf> {
    let parent_dir = base_path.as_ref().parent()?;
    let resolved_path = parent_dir.join(include_path);
    if resolved_path.exists() {
        Some(resolved_path)
    } else {
        None
    }
}
