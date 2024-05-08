use std::ops::ControlFlow;
use std::sync::{Arc, RwLock};

use async_lsp::lsp_types::{
    notification, request, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, InitializeParams, InitializeResult, OneOf,
    PublishDiagnosticsParams, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
    Url,
};
use async_lsp::router::Router;
use async_lsp::{ClientSocket, LanguageClient, LanguageServer, ResponseError};
use futures::future::{ready, BoxFuture};

use ide::analysis::{Analysis, AnalysisHost};
use ide::file_system::FileSystem;
use tokio::task::{self};

use crate::vfs::{UrlExt, Vfs};
use crate::{from_proto, to_proto};

pub struct Server {
    host: AnalysisHost,
    vfs: Arc<RwLock<Vfs>>,
    client: ClientSocket,
    diagnostic_version: i32,
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
            .notification::<notification::DidOpenTextDocument>(Self::did_open)
            .notification::<notification::DidChangeTextDocument>(Self::did_change)
            .notification::<notification::DidSaveTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidCloseTextDocument>(|_, _| ControlFlow::Continue(()))
            .request::<request::DocumentSymbolRequest, _>(Self::document_symbol);
        router
    }

    fn new(client: ClientSocket) -> Self {
        Self {
            host: AnalysisHost::new(),
            vfs: Arc::new(RwLock::new(Vfs::new())),
            client,
            diagnostic_version: 0,
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
                document_symbol_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
        })))
    }

    fn document_symbol(
        &mut self,
        params: DocumentSymbolParams,
    ) -> BoxFuture<'static, Result<Option<DocumentSymbolResponse>, Self::Error>> {
        tracing::info!("document_symbol: {params:?}");
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (file_id, line_index) = from_proto::file(&snap, params.text_document);
            let Some(symbols) = snap.analysis.document_symbol(file_id) else {
                return Ok(None);
            };

            let lsp_symbols = symbols
                .into_iter()
                .map(|it| to_proto::document_symbol(&line_index, it))
                .collect();
            Ok(Some(DocumentSymbolResponse::Nested(lsp_symbols)))
        });
        Box::pin(async move { task.await.unwrap() })
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> Self::NotifyResult {
        self.set_file_content(&params.text_document.uri, &params.text_document.text);
        self.update_diagnostics();
        ControlFlow::Continue(())
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> Self::NotifyResult {
        if let Some(change) = params.content_changes.first() {
            self.set_file_content(&params.text_document.uri, &change.text);
            self.update_diagnostics();
        }
        ControlFlow::Continue(())
    }
}

impl Server {
    fn set_file_content(&mut self, uri: &Url, text: &str) {
        let path = UrlExt::to_file_path(uri);
        let mut vfs = self.vfs.write().unwrap();
        let file_id = vfs.assign_or_get_file_id(path);
        let text = Arc::from(text);
        self.host.set_file_content(file_id, text);
        self.host.set_root_file(&mut *vfs, file_id);
    }

    fn update_diagnostics(&mut self) {
        let diag_version = self.bump_diagnostic_version();
        let mut client = self.client.clone();
        self.spawn_with_snapshot((), move |snap, _| {
            for (file_id, diagnostics) in snap.analysis.diagnostics() {
                let line_index = snap.analysis.line_index(file_id);
                let lsp_diags = diagnostics
                    .into_iter()
                    .map(|diag| to_proto::diagnostic(&line_index, diag))
                    .collect();

                let vfs = snap.vfs.read().unwrap();
                let file_path = vfs.path_for_file(&file_id);
                let file_uri = UrlExt::from_file_path(file_path);

                let params = PublishDiagnosticsParams::new(file_uri, lsp_diags, Some(diag_version));
                client
                    .publish_diagnostics(params)
                    .expect("failed to publish diagnostics");
            }
        });
    }

    fn bump_diagnostic_version(&mut self) -> i32 {
        let version = self.diagnostic_version;
        self.diagnostic_version += 1;
        version
    }

    fn spawn_with_snapshot<P: Send + 'static, T: Send + 'static>(
        &mut self,
        params: P,
        f: impl FnOnce(ServerSnapshot, P) -> T + Send + 'static,
    ) -> task::JoinHandle<T> {
        let snap = ServerSnapshot {
            analysis: self.host.analysis(),
            vfs: Arc::clone(&self.vfs),
        };
        task::spawn_blocking(move || f(snap, params))
    }
}

pub struct ServerSnapshot {
    pub analysis: Analysis,
    pub vfs: Arc<RwLock<Vfs>>,
}
