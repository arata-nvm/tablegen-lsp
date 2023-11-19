use async_lsp::stdio::{PipeStdin, PipeStdout};
use tablegen_lsp::server::TableGenLanguageServer;
use tower::ServiceBuilder;

#[tokio::main]
async fn main() {
    let stdin = PipeStdin::lock_tokio().unwrap();
    let stdout = PipeStdout::lock_tokio().unwrap();

    let (mainloop, _) = async_lsp::MainLoop::new_server(|client| {
        ServiceBuilder::new().service(TableGenLanguageServer::new_router(client))
    });

    mainloop.run_buffered(stdin, stdout).await.unwrap();
}
