use tablegen_analyzer::{analyzer, document::TableGenDocument};
use tokio::sync::Mutex;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentSymbolParams,
        DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse, InitializeParams,
        InitializeResult, OneOf, ServerCapabilities, TextDocumentSyncCapability,
        TextDocumentSyncKind, Url,
    },
    Client, LanguageServer,
};

use crate::{
    compat::{analyzer2lsp, lsp2analyzer},
    document_map::TableGenDocumentMap,
};

pub struct TableGenLanguageServer {
    client: Client,
    document_map: Mutex<TableGenDocumentMap>,
}

impl TableGenLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            document_map: Mutex::new(TableGenDocumentMap::new()),
        }
    }

    async fn on_change(&self, uri: Url, version: i32, text: String) {
        let mut document_map = self.document_map.lock().await;
        let doc_id = document_map.assign_document_id(uri.clone());
        let mut document = TableGenDocument::parse(doc_id, text);

        let diags = document
            .take_errors()
            .into_iter()
            .map(|error| analyzer2lsp::error(&document, error))
            .collect();
        self.client
            .publish_diagnostics(uri, diags, Some(version))
            .await;

        document_map.update_document(doc_id, document);
    }

    async fn with_document<T>(
        &self,
        uri: Url,
        f: impl FnOnce(&TableGenDocumentMap, &TableGenDocument) -> Option<T>,
    ) -> Option<T> {
        let document_map = self.document_map.lock().await;
        let Some(doc_id) = document_map.to_document_id(&uri) else { return None; };
        let Some(document) = document_map.find_document(doc_id) else { return None; };
        f(&document_map, document)
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for TableGenLanguageServer {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                definition_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(
            params.text_document.uri,
            params.text_document.version,
            params.text_document.text,
        )
        .await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(
            params.text_document.uri,
            params.text_document.version,
            std::mem::take(&mut params.content_changes[0].text),
        )
        .await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let definition = self
            .with_document(uri, |doc_map, doc| {
                let lsp_position = params.text_document_position_params.position;
                let position = lsp2analyzer::position(doc, lsp_position);
                let definition = doc.get_definition(position)?;
                let lsp_definition = analyzer2lsp::location(doc_map, doc, definition);
                Some(GotoDefinitionResponse::Scalar(lsp_definition))
            })
            .await;

        Ok(definition)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        let symbols = self
            .with_document(uri, |_, doc| {
                let symbols = analyzer::document_symbol::document_symbol(doc)?;
                let lsp_symbols = symbols
                    .into_iter()
                    .map(|symbol| analyzer2lsp::document_symbol(doc, symbol))
                    .collect();
                Some(DocumentSymbolResponse::Nested(lsp_symbols))
            })
            .await;

        Ok(symbols)
    }
}
