use tablegen_lsp::server::TableGenLanguageServer;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(TableGenLanguageServer::new).finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
