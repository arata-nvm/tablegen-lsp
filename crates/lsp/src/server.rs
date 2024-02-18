use std::ops::ControlFlow;

use async_lsp::lsp_types::{notification, request, InitializeParams, InitializeResult};
use async_lsp::router::Router;
use async_lsp::{ClientSocket, LanguageServer, ResponseError};
use futures::future::{ready, BoxFuture};

pub struct Server {
    client: ClientSocket,
}

impl Server {
    pub fn new_router(client: ClientSocket) -> Router<Self> {
        let this = Self::new(client);
        let mut router = Router::new(this);
        router
            .request::<request::Initialize, _>(Self::initialize)
            .notification::<notification::Initialized>(|_, _| ControlFlow::Continue(()));
        router
    }

    fn new(client: ClientSocket) -> Self {
        Self { client }
    }
}

impl LanguageServer for Server {
    type Error = ResponseError;
    type NotifyResult = ControlFlow<async_lsp::Result<()>>;

    fn initialize(
        &mut self,
        params: InitializeParams,
    ) -> BoxFuture<'static, Result<InitializeResult, Self::Error>> {
        tracing::info!("initialize: {params:?}");
        Box::pin(ready(Ok(InitializeResult::default())))
    }
}
