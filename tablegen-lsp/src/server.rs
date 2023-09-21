use tower_lsp::{
    jsonrpc::Result,
    lsp_types::{InitializeParams, InitializeResult},
    Client, LanguageServer,
};

pub struct TableGenLanguageServer {
    client: Client,
}

impl TableGenLanguageServer {
    pub fn new(client: Client) -> Self {
        Self { client }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for TableGenLanguageServer {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult::default())
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}
