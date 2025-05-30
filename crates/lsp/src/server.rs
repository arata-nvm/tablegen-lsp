use std::ops::ControlFlow;
use std::sync::{Arc, RwLock};

use async_lsp::lsp_types::{
    notification, request, CompletionOptions, CompletionParams, CompletionResponse,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentLink, DocumentLinkOptions,
    DocumentLinkParams, DocumentSymbolParams, DocumentSymbolResponse, FoldingRange,
    FoldingRangeParams, FoldingRangeProviderCapability, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverParams, HoverProviderCapability, InitializeParams,
    InitializeResult, InlayHint, InlayHintParams, Location, OneOf, PublishDiagnosticsParams,
    ReferenceParams, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use async_lsp::router::Router;
use async_lsp::{ClientSocket, LanguageClient, LanguageServer, ResponseError};
use futures::future::{ready, BoxFuture};
use tokio::task::{self};

use ide::analysis::{Analysis, AnalysisHost};
use ide::file_system::FileSystem;

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
            .request::<request::DocumentSymbolRequest, _>(Self::document_symbol)
            .request::<request::GotoDefinition, _>(Self::definition)
            .request::<request::References, _>(Self::references)
            .request::<request::HoverRequest, _>(Self::hover)
            .request::<request::InlayHintRequest, _>(Self::inlay_hint)
            .request::<request::Completion, _>(Self::completion)
            .request::<request::DocumentLinkRequest, _>(Self::document_link)
            .request::<request::FoldingRangeRequest, _>(Self::folding_range);
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
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![String::from("!")]), // for bang operator
                    ..Default::default()
                }),
                document_link_provider: Some(DocumentLinkOptions {
                    resolve_provider: Some(true),
                    work_done_progress_options: Default::default(),
                }),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                ..Default::default()
            },
        })))
    }

    fn hover(
        &mut self,
        params: HoverParams,
    ) -> BoxFuture<'static, Result<Option<Hover>, Self::Error>> {
        tracing::info!("hover: {params:?}");
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (pos, _) = from_proto::file_pos(&snap, params.text_document_position_params);
            let Some(hover) = snap.analysis.hover(pos) else {
                return Ok(None);
            };
            let lsp_hover = to_proto::hover(hover);
            Ok(Some(lsp_hover))
        });
        Box::pin(async move { task.await.unwrap() })
    }

    fn definition(
        &mut self,
        params: GotoDefinitionParams,
    ) -> BoxFuture<'static, Result<Option<GotoDefinitionResponse>, Self::Error>> {
        tracing::info!("goto_definition: {params:?}");
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (pos, line_index) =
                from_proto::file_pos(&snap, params.text_document_position_params);
            let Some(location) = snap.analysis.goto_definition(pos) else {
                return Ok(None);
            };

            let vfs = snap.vfs.read().unwrap();
            let lsp_location = to_proto::location(&vfs, &line_index, location);
            Ok(Some(GotoDefinitionResponse::Scalar(lsp_location)))
        });
        Box::pin(async move { task.await.unwrap() })
    }

    fn references(
        &mut self,
        params: ReferenceParams,
    ) -> BoxFuture<'static, Result<Option<Vec<Location>>, Self::Error>> {
        tracing::info!("references: {params:?}");
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (pos, line_index) = from_proto::file_pos(&snap, params.text_document_position);
            let Some(location_list) = snap.analysis.references(pos) else {
                return Ok(None);
            };
            let vfs = snap.vfs.read().unwrap();
            let lsp_location_list = location_list
                .into_iter()
                .map(|it| to_proto::location(&vfs, &line_index, it))
                .collect();
            Ok(Some(lsp_location_list))
        });
        Box::pin(async move { task.await.unwrap() })
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

    fn inlay_hint(
        &mut self,
        params: InlayHintParams,
    ) -> BoxFuture<'static, Result<Option<Vec<InlayHint>>, Self::Error>> {
        tracing::info!("inlay_hint: {params:?}");
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (range, line_index) =
                from_proto::file_range(&snap, params.text_document, params.range);
            let Some(inlay_hints) = snap.analysis.inlay_hint(range) else {
                return Ok(None);
            };

            let lsp_inlay_hints = inlay_hints
                .into_iter()
                .map(|it| to_proto::inlay_hint(&line_index, it))
                .collect();
            Ok(Some(lsp_inlay_hints))
        });
        Box::pin(async move { task.await.unwrap() })
    }

    fn completion(
        &mut self,
        params: CompletionParams,
    ) -> BoxFuture<'static, Result<Option<CompletionResponse>, Self::Error>> {
        tracing::info!("completion: {params:?}");
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (pos, _) = from_proto::file_pos(&snap, params.text_document_position);
            let trigger_char = params.context.and_then(|it| it.trigger_character);
            let Some(completion_list) = snap.analysis.completion(pos, trigger_char) else {
                return Ok(None);
            };

            let lsp_completion_list = completion_list
                .into_iter()
                .map(to_proto::completion_item)
                .collect();
            Ok(Some(CompletionResponse::Array(lsp_completion_list)))
        });
        Box::pin(async move { task.await.unwrap() })
    }

    fn document_link(
        &mut self,
        params: DocumentLinkParams,
    ) -> BoxFuture<'static, Result<Option<Vec<DocumentLink>>, Self::Error>> {
        tracing::info!("document link: {params:?}");
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (file_id, line_index) = from_proto::file(&snap, params.text_document);
            let Some(links) = snap.analysis.document_link(file_id) else {
                return Ok(None);
            };

            let vfs = snap.vfs.read().unwrap();
            let lsp_links = links
                .into_iter()
                .map(|it| to_proto::document_link(&vfs, &line_index, it))
                .collect();
            Ok(Some(lsp_links))
        });
        Box::pin(async move { task.await.unwrap() })
    }

    fn folding_range(
        &mut self,
        params: FoldingRangeParams,
    ) -> BoxFuture<'static, Result<Option<Vec<FoldingRange>>, Self::Error>> {
        tracing::info!("folding_range: {params:?}");
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (file_id, line_index) = from_proto::file(&snap, params.text_document);
            let Some(folding_ranges) = snap.analysis.folding_range(file_id) else {
                return Ok(None);
            };

            let lsp_folding_ranges = folding_ranges
                .into_iter()
                .map(|it| to_proto::folding_range(&line_index, it))
                .collect();
            Ok(Some(lsp_folding_ranges))
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
