use std::ops::ControlFlow;
use std::sync::Arc;

use async_lsp::lsp_types::{
    notification, request, DidOpenTextDocumentParams, InitializeParams, InitializeResult,
    PublishDiagnosticsParams, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
    Url,
};
use async_lsp::router::Router;
use async_lsp::{ClientSocket, LanguageClient, LanguageServer, ResponseError};
use futures::future::{ready, BoxFuture};

use ide::analysis::AnalysisHost;
use ide::db::SourceDatabase;
use ide::file::FileId;

use crate::to_proto;
use crate::vfs::{UrlExt, Vfs};

pub struct Server {
    host: AnalysisHost,
    vfs: Vfs,
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
            .notification::<notification::DidOpenTextDocument>(Self::did_open);
        router
    }

    fn new(client: ClientSocket) -> Self {
        Self {
            host: AnalysisHost::new(),
            vfs: Vfs::new(),
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
                ..Default::default()
            },
        })))
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> Self::NotifyResult {
        let file_id = self.set_file_content(&params.text_document.uri, &params.text_document.text);
        self.update_diagnostics(file_id);
        ControlFlow::Continue(())
    }
}

impl Server {
    fn set_file_content(&mut self, uri: &Url, text: &str) -> FileId {
        let path = UrlExt::to_file_path(uri);
        let file_id = self.vfs.assign_or_get_file_id(path);
        let text = Arc::from(text);
        self.host.set_file_content(file_id, text);
        file_id
    }

    fn update_diagnostics(&mut self, file_id: FileId) {
        let Some(file_path) = self.vfs.path_for_file(&file_id) else {
            tracing::info!("cannot retrieve file path: {file_id:?}");
            return;
        };
        let file_uri = UrlExt::from_file_path(&file_path);

        let analysis = self.host.analysis();
        let diags = analysis.diagnostics(file_id);
        let line_index = analysis.snapshot().line_index(file_id);
        let lsp_diags = diags
            .into_iter()
            .filter_map(|diag| to_proto::diagnostic(&line_index, diag))
            .collect();

        let params = PublishDiagnosticsParams::new(
            file_uri,
            lsp_diags,
            Some(self.bump_diagnostic_version()),
        );
        self.client.publish_diagnostics(params).unwrap();
    }

    fn bump_diagnostic_version(&mut self) -> i32 {
        let version = self.diagnostic_version;
        self.diagnostic_version += 1;
        version
    }
}
