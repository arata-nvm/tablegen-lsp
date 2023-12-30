use std::fs;
use std::path::PathBuf;

use async_lsp::LanguageClient;
use lsp_types::{PublishDiagnosticsParams, Url};

use tablegen_analyzer::document::{Document, DocumentId};
use tablegen_analyzer::source::{Dependencies, Source, SourceSet};
use tablegen_parser::ast::AstNode;
use tablegen_parser::language::SyntaxNode;
use tablegen_parser::{ast, grammar};

use crate::compat::analyzer2lsp;
use crate::server::TableGenLanguageServer;

impl TableGenLanguageServer {
    pub(super) fn check_file(&mut self, uri: Url, version: i32, text: String) {
        let doc_id = self.document_map.assign_document_id(uri.clone());

        let extract_include_paths = |root: SyntaxNode| {
            let root = ast::Root::cast(root).unwrap();
            let statement_list = root.statement_list().unwrap();
            statement_list
                .statements()
                .filter_map(|stmt| match stmt {
                    ast::Statement::Include(include) => Some(include),
                    _ => None,
                })
                .filter_map(|include| include.path())
                .map(|path| path.value())
                .collect::<Vec<String>>()
        };

        let resolve_path = |path: &str, base_path: PathBuf| {
            let parent_dir = base_path.parent()?;
            let resolved_path = parent_dir.join(path);
            if resolved_path.exists() {
                Some(resolved_path)
            } else {
                None
            }
        };

        let mut list_dependencies =
            |text: &str, text_path: PathBuf, dependencies: &mut Dependencies| {
                let (root, _) = grammar::parse(text);
                let include_paths = extract_include_paths(root);
                for include_path in include_paths {
                    let Some(resolved_path) = resolve_path(&include_path, text_path.clone()) else {
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
            };

        let mut to_source_set = |uri: Url, doc_id: DocumentId, doc_text: String| {
            let text_path = PathBuf::from(uri.path());
            let mut dependencies = Dependencies::new();
            list_dependencies(&doc_text, text_path, &mut dependencies);
            SourceSet::new(Source::new(doc_id, doc_text), dependencies)
        };

        let source_set = to_source_set(uri.clone(), doc_id, text);
        let mut document = Document::parse_set(source_set);

        let diags = document
            .take_errors()
            .into_iter()
            .map(|error| analyzer2lsp::error(&document, error))
            .collect();
        self.client
            .publish_diagnostics(PublishDiagnosticsParams::new(uri, diags, Some(version)))
            .unwrap();

        self.document_map.update_document(doc_id, document);
    }
}
