use std::collections::{HashMap, HashSet};
use std::ops::ControlFlow;
use std::sync::{Arc, RwLock};

use async_lsp::lsp_types::{
    notification, request, CompletionOptions, CompletionParams,
    CompletionResponse, Diagnostic, DidChangeTextDocumentParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DocumentLink, DocumentLinkOptions,
    DocumentLinkParams, DocumentSymbolParams, DocumentSymbolResponse, FoldingRange,
    FoldingRangeParams, FoldingRangeProviderCapability, GotoDefinitionParams, GotoDefinitionResponse, Hover,
    HoverParams, HoverProviderCapability, InitializeParams, InitializeResult, InlayHint, InlayHintParams, Location,
    MessageType, OneOf, PublishDiagnosticsParams, ReferenceParams, ServerCapabilities,
    ServerInfo, ShowMessageParams, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use async_lsp::router::Router;
use async_lsp::{ClientSocket, LanguageClient, LanguageServer, ResponseError};
use futures::future::{ready, BoxFuture};
use serde_json::Value;
use tokio::task::{self};

use ide::analysis::{Analysis, AnalysisHost};
use ide::file_system::{FileId, FileSystem, SourceUnitId};

use crate::config::Config;
use crate::vfs::{UrlExt, Vfs};
use crate::{from_proto, lsp_ext, to_proto};

pub struct Server {
    host: AnalysisHost,
    vfs: Arc<RwLock<Vfs>>,
    client: ClientSocket,
    config: Arc<Config>,

    diagnostic_version: i32,
    opened_source_units: HashSet<SourceUnitId>,
    root_source_unit: Option<SourceUnitId>,
}

pub struct ServerSnapshot {
    pub analysis: Analysis,
    pub vfs: Arc<RwLock<Vfs>>,
}

struct UpdateDiagnosticsEvent(i32, HashMap<FileId, Vec<Diagnostic>>);
struct UpdateConfigEvent(Value);

impl Server {
    pub fn new_router(client: ClientSocket) -> Router<Self> {
        let this = Self::new(client);
        let mut router = Router::new(this);
        router
            .request::<request::Initialize, _>(Self::initialize)
            .request::<request::Shutdown, _>(|_, _| ready(Ok(())))
            .notification::<notification::Initialized>(Self::initialized)
            .notification::<notification::Exit>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidOpenTextDocument>(Self::did_open)
            .notification::<notification::DidChangeTextDocument>(Self::did_change)
            .notification::<notification::DidSaveTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidCloseTextDocument>(Self::did_close)
            .notification::<lsp_ext::SetSourceRoot>(Self::set_source_root)
            .notification::<lsp_ext::ClearSourceRoot>(Self::clear_source_root)
            .request::<request::DocumentSymbolRequest, _>(Self::document_symbol)
            .request::<request::GotoDefinition, _>(Self::definition)
            .request::<request::References, _>(Self::references)
            .request::<request::HoverRequest, _>(Self::hover)
            .request::<request::InlayHintRequest, _>(Self::inlay_hint)
            .request::<request::Completion, _>(Self::completion)
            .request::<request::DocumentLinkRequest, _>(Self::document_link)
            .request::<request::FoldingRangeRequest, _>(Self::folding_range)
            .event::<UpdateDiagnosticsEvent>(Self::update_diagnostics)
            .event::<UpdateConfigEvent>(Self::update_config);
        router
    }

    fn new(client: ClientSocket) -> Self {
        Self {
            host: AnalysisHost::new(),
            vfs: Arc::new(RwLock::new(Vfs::new())),
            client,
            config: Arc::new(Config::default()),
            diagnostic_version: 0,
            opened_source_units: HashSet::new(),
            root_source_unit: None,
        }
    }
}

// request, notificationのハンドラー
impl LanguageServer for Server {
    type Error = ResponseError;
    type NotifyResult = ControlFlow<async_lsp::Result<()>>;

    fn initialize(
        &mut self,
        params: InitializeParams,
    ) -> BoxFuture<'static, Result<InitializeResult, Self::Error>> {
        tracing::info!("initialize: {params:?}");

        // TODO: It seems that did_change_configuration is not called, so we need to update the config here.
        if let Some(options) = params.initialization_options {
            if options.as_object().filter(|it| !it.is_empty()).is_some() {
                let _ = self.update_config(UpdateConfigEvent(options));
            }
        }

        Box::pin(ready(Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "tablegen-lsp".into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
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

    fn inlay_hint(
        &mut self,
        params: InlayHintParams,
    ) -> BoxFuture<'static, Result<Option<Vec<InlayHint>>, Self::Error>> {
        tracing::info!("inlay_hint: {params:?}");
        let source_unit_id = self.current_source_unit(&params.text_document.uri);
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (range, line_index) =
                from_proto::file_range(&snap, params.text_document, params.range);
            let Some(inlay_hints) = snap.analysis.inlay_hint(source_unit_id, range) else {
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
        let source_unit_id =
            self.current_source_unit(&params.text_document_position.text_document.uri);
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (pos, _) = from_proto::file_pos(&snap, params.text_document_position);
            let trigger_char = params.context.and_then(|it| it.trigger_character);
            let Some(completion_list) = snap.analysis.completion(source_unit_id, pos, trigger_char)
            else {
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

    fn hover(
        &mut self,
        params: HoverParams,
    ) -> BoxFuture<'static, Result<Option<Hover>, Self::Error>> {
        tracing::info!("hover: {params:?}");
        let source_unit_id =
            self.current_source_unit(&params.text_document_position_params.text_document.uri);
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (pos, _) = from_proto::file_pos(&snap, params.text_document_position_params);
            let Some(hover) = snap.analysis.hover(source_unit_id, pos) else {
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
        let source_unit_id =
            self.current_source_unit(&params.text_document_position_params.text_document.uri);
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (pos, _) = from_proto::file_pos(&snap, params.text_document_position_params);
            let Some(location) = snap.analysis.goto_definition(source_unit_id, pos) else {
                return Ok(None);
            };

            let lsp_location = to_proto::location(&snap, location);
            Ok(Some(GotoDefinitionResponse::Scalar(lsp_location)))
        });
        Box::pin(async move { task.await.unwrap() })
    }

    fn references(
        &mut self,
        params: ReferenceParams,
    ) -> BoxFuture<'static, Result<Option<Vec<Location>>, Self::Error>> {
        tracing::info!("references: {params:?}");
        let source_unit_id =
            self.current_source_unit(&params.text_document_position.text_document.uri);
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (pos, _) = from_proto::file_pos(&snap, params.text_document_position);
            let Some(location_list) = snap.analysis.references(source_unit_id, pos) else {
                return Ok(None);
            };

            let lsp_location_list = location_list
                .into_iter()
                .map(|it| to_proto::location(&snap, it))
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
        let source_unit_id = self.current_source_unit(&params.text_document.uri);
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (file_id, line_index) = from_proto::file(&snap, params.text_document);
            let Some(symbols) = snap.analysis.document_symbol(source_unit_id, file_id) else {
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

    fn document_link(
        &mut self,
        params: DocumentLinkParams,
    ) -> BoxFuture<'static, Result<Option<Vec<DocumentLink>>, Self::Error>> {
        tracing::info!("document link: {params:?}");
        let source_unit_id = self.current_source_unit(&params.text_document.uri);
        let task = self.spawn_with_snapshot(params, move |snap, params| {
            let (file_id, line_index) = from_proto::file(&snap, params.text_document);
            let Some(links) = snap.analysis.document_link(source_unit_id, file_id) else {
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

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> Self::NotifyResult {
        tracing::info!("did_open: {params:?}");
        let source_unit_id =
            self.load_source_unit(&params.text_document.uri, &params.text_document.text);
        self.opened_source_units.insert(source_unit_id);
        self.spawn_update_diagnostics();
        ControlFlow::Continue(())
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> Self::NotifyResult {
        tracing::info!("did_change: {params:?}");
        if let Some(change) = params.content_changes.first() {
            self.load_source_unit(&params.text_document.uri, &change.text);
            self.spawn_update_diagnostics();
        }
        ControlFlow::Continue(())
    }

    fn did_close(&mut self, params: DidCloseTextDocumentParams) -> Self::NotifyResult {
        tracing::info!("did_close: {params:?}");
        {
            let path = UrlExt::to_file_path(&params.text_document.uri);
            let vfs = self.vfs.read().unwrap();
            let Some(file_id) = vfs.file_for_path(&path) else {
                tracing::warn!("cannot find file id: {path:?}");
                return ControlFlow::Continue(());
            };
            let source_unit_id = SourceUnitId::from_root_file(file_id);
            self.opened_source_units.remove(&source_unit_id);
        }
        self.spawn_update_diagnostics();
        ControlFlow::Continue(())
    }
}

// lsp_extのハンドラー
impl Server {
    fn set_source_root(
        &mut self,
        params: lsp_ext::SetSourceRootParams,
    ) -> <Self as LanguageServer>::NotifyResult {
        tracing::info!("set_source_root: {params:?}");
        let content = {
            let path = UrlExt::to_file_path(&params.uri);
            let vfs = self.vfs.read().unwrap();
            let Some(content) = vfs.read_content(&path) else {
                tracing::warn!("failed to read file: {path:?}");
                return ControlFlow::Continue(());
            };
            content
        };
        let source_unit_id = self.load_source_unit(&params.uri, &content);
        self.root_source_unit.replace(source_unit_id);
        self.spawn_update_diagnostics();
        ControlFlow::Continue(())
    }

    fn clear_source_root(
        &mut self,
        _params: lsp_ext::ClearSourceRootParams,
    ) -> <Self as LanguageServer>::NotifyResult {
        tracing::info!("clear_source_root");
        self.root_source_unit.take();
        self.spawn_update_diagnostics();
        ControlFlow::Continue(())
    }
}

impl Server {
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

    fn spawn_update_diagnostics(&mut self) {
        let diag_version = self.bump_diagnostic_version();
        let client = self.client.clone();
        let opened_source_units = match self.root_source_unit {
            Some(source_unit_id) => HashSet::from([source_unit_id]),
            None => self.opened_source_units.clone(),
        };

        self.spawn_with_snapshot((), move |snap, _| {
            let mut diags = HashMap::new();
            for source_unit_id in opened_source_units {
                for (file_id, diagnostics) in snap.analysis.diagnostics(source_unit_id) {
                    let line_index = snap.analysis.line_index(file_id);
                    let lsp_diags: Vec<Diagnostic> = diagnostics
                        .into_iter()
                        .map(|diag| to_proto::diagnostic(&line_index, diag))
                        .collect();

                    diags.entry(file_id).or_insert(Vec::new()).extend(lsp_diags);
                }
            }

            client
                .emit(UpdateDiagnosticsEvent(diag_version, diags))
                .expect("failed to emit update diagnostics event");
        });
    }

    fn bump_diagnostic_version(&mut self) -> i32 {
        let version = self.diagnostic_version;
        self.diagnostic_version += 1;
        version
    }

    fn update_diagnostics(
        &mut self,
        UpdateDiagnosticsEvent(version, diagnostics): UpdateDiagnosticsEvent,
    ) -> <Self as LanguageServer>::NotifyResult {
        tracing::info!("update_diagnostics: {version:?}");
        for (file_id, lsp_diags) in diagnostics {
            let vfs = self.vfs.read().unwrap();
            let file_path = vfs.path_for_file(&file_id);
            let file_uri = UrlExt::from_file_path(file_path);

            let params = PublishDiagnosticsParams::new(file_uri, lsp_diags, Some(version));
            self.client
                .publish_diagnostics(params)
                .expect("failed to publish diagnostics");
        }
        ControlFlow::Continue(())
    }

    fn update_config(
        &mut self,
        UpdateConfigEvent(v): UpdateConfigEvent,
    ) -> <Self as LanguageServer>::NotifyResult {
        tracing::info!("update_config: {v:?}");

        let config = Arc::get_mut(&mut self.config).expect("cannot get mutable reference");
        if let Err(err) = config.update(v) {
            self.client
                .show_message(ShowMessageParams {
                    typ: MessageType::ERROR,
                    message: format!("failed to reload config: {err}"),
                })
                .expect("failed to show message");
        }

        ControlFlow::Continue(())
    }

    fn load_source_unit(&mut self, uri: &Url, text: &str) -> SourceUnitId {
        let path = UrlExt::to_file_path(uri);
        let mut vfs = self.vfs.write().unwrap();
        let file_id = vfs.assign_or_get_file_id(path);
        let text = Arc::from(text);
        self.host.set_file_content(file_id, text);
        self.host
            .load_source_unit(&mut *vfs, file_id, &self.config.include_dirs)
    }

    fn current_source_unit(&mut self, uri: &Url) -> SourceUnitId {
        match self.root_source_unit {
            Some(source_unit_id) => source_unit_id,
            None => {
                let path = UrlExt::to_file_path(uri);
                let mut vfs = self.vfs.write().unwrap();
                let file_id = vfs.assign_or_get_file_id(path);
                SourceUnitId::from_root_file(file_id)
            }
        }
    }
}
