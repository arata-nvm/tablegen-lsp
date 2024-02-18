use async_lsp::router::Router;
use async_lsp::ClientSocket;

pub struct Server {
    client: ClientSocket,
}

impl Server {
    pub fn new_router(client: ClientSocket) -> Router<Self> {
        let this = Self::new(client);
        Router::new(this)
    }

    fn new(client: ClientSocket) -> Self {
        Self { client }
    }
}
