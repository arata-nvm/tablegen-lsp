use tower::ServiceBuilder;

use lsp::server::Server;

#[tokio::main]
async fn main() {
    let (stdin, stdout) = (
        async_lsp::stdio::PipeStdin::lock_tokio().unwrap(),
        async_lsp::stdio::PipeStdout::lock_tokio().unwrap(),
    );

    let (mainloop, _) = async_lsp::MainLoop::new_server(|client| {
        ServiceBuilder::new().service(Server::new_router(client))
    });

    mainloop.run_buffered(stdin, stdout).await.unwrap();
}
