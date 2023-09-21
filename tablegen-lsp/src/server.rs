use dashmap::DashMap;
use tablegen_analyzer::document::TableGenDocument;
use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, GotoDefinitionParams,
        GotoDefinitionResponse, InitializeParams, InitializeResult, OneOf, ServerCapabilities,
        TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    },
    Client, LanguageServer,
};

pub struct TableGenLanguageServer {
    client: Client,
    documents: DashMap<Url, TableGenDocument>,
}

impl TableGenLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
        }
    }

    async fn on_change(&self, uri: Url, version: i32, text: String) {
        let mut document = TableGenDocument::parse(uri.clone(), text);
        self.client
            .publish_diagnostics(uri.clone(), document.take_diagnostics(), Some(version))
            .await;
        self.documents.insert(uri, document);
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
        let position = params.text_document_position_params.position;

        let Some(document) = self.documents.get(&uri) else { return Ok(None); };
        let definition = document.get_definition(position);
        Ok(definition.map(GotoDefinitionResponse::Scalar))
    }
}
