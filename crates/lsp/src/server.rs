use std::ops::ControlFlow;

use async_lsp::lsp_types::{
    notification, request, DidOpenTextDocumentParams, InitializeParams, InitializeResult,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};
use async_lsp::router::Router;
use async_lsp::{ClientSocket, LanguageServer, ResponseError};
use futures::future::{ready, BoxFuture};

use crate::vfs::{UrlExt, Vfs};

pub struct Server {
    vfs: Vfs,
    client: ClientSocket,
}

impl Server {
    pub fn new_router(client: ClientSocket) -> Router<Self> {
        let this = Self::new(client);
        let mut router = Router::new(this);
        router
            .request::<request::Initialize, _>(Self::initialize)
            .notification::<notification::Initialized>(|_, _| ControlFlow::Continue(()))
            .request::<request::Shutdown, _>(|_, _| ready(Ok(())))
            .notification::<notification::Exit>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidOpenTextDocument>(Self::did_open);
        router
    }

    fn new(client: ClientSocket) -> Self {
        Self {
            vfs: Vfs::new(),
            client,
        }
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
        Box::pin(ready(Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..Default::default()
            },
        })))
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> Self::NotifyResult {
        let path = UrlExt::to_file_path(&params.text_document.uri);
        let file_id = self.vfs.assign_or_get_file_id(path);

        ControlFlow::Continue(())
    }
}
