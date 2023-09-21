use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, InitializeParams, InitializeResult,
        ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    },
    Client, LanguageServer,
};

use crate::document::TableGenDocument;

pub struct TableGenLanguageServer {
    client: Client,
}

impl TableGenLanguageServer {
    pub fn new(client: Client) -> Self {
        Self { client }
    }

    async fn on_change(&self, uri: Url, version: i32, text: String) {
        let mut document = TableGenDocument::parse(uri.clone(), text);
        self.client
            .publish_diagnostics(uri, document.take_diagnostics(), Some(version))
            .await;
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
}
