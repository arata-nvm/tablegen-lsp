use async_lsp::stdio::{PipeStdin, PipeStdout};
use tower::ServiceBuilder;

use tablegen_lsp::server::TableGenLanguageServer;

#[tokio::main]
async fn main() {
    #[cfg(unix)]
    let (stdin, stdout) = (
        PipeStdin::lock_tokio().unwrap(),
        PipeStdout::lock_tokio().unwrap(),
    );

    #[cfg(not(unix))]
    let (stdin, stdout) = (
        tokio_util::compat::TokioAsyncReadCompatExt::compat(tokio::io::stdin()),
        tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(tokio::io::stdout()),
    );

    let (mainloop, _) = async_lsp::MainLoop::new_server(|client| {
        ServiceBuilder::new().service(TableGenLanguageServer::new_router(client))
    });

    mainloop.run_buffered(stdin, stdout).await.unwrap();
}
