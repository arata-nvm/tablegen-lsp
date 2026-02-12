use std::borrow::BorrowMut;
use std::collections::HashSet;
use std::ops::ControlFlow;
use std::sync::Arc;

use async_lsp::lsp_types::{
    CompletionOptions, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentLinkOptions,
    FoldingRangeProviderCapability, HoverProviderCapability, InitializeParams, InitializeResult,
    MessageType, OneOf, ProgressParams, ProgressParamsValue, ProgressToken,
    PublishDiagnosticsParams, ServerCapabilities, ServerInfo, ShowMessageParams,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkDoneProgress, WorkDoneProgressBegin,
    WorkDoneProgressCreateParams, WorkDoneProgressEnd, notification, request,
};
use async_lsp::router::Router;
use async_lsp::{ClientSocket, ErrorCode, LanguageClient, LanguageServer, ResponseError};
use futures::future::{BoxFuture, ready};
use ide::interop::{self, TblgenParseResult};
use serde_json::Value;
use tokio::task::{self};

use ide::analysis::{Analysis, AnalysisHost, Cancellable};
use ide::file_system::{FileSystem, SourceUnitId};

use crate::config::Config;
use crate::diagnostics::DiagnosticCollection;
use crate::vfs::{SharedFs, UrlExt, Vfs};
use crate::{handlers, lsp_ext};

pub struct Server {
    host: AnalysisHost,
    vfs: SharedFs<Vfs>,
    client: ClientSocket,
    config: Arc<Config>,

    diagnostic_version: i32,
    opened_source_units: HashSet<SourceUnitId>,
    root_source_unit: Option<SourceUnitId>,

    flycheck_task: Option<task::JoinHandle<()>>,
}

const FLYCHECK_PROGRESS_TOKEN: &str = "tablegen-lsp/flycheck";

pub type Result<T> = std::result::Result<T, ResponseError>;

struct UpdateDiagnosticsEvent(i32, DiagnosticCollection);
struct UpdateConfigEvent(Value);
struct UpdateFlycheckEvent(SourceUnitId, TblgenParseResult);

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
            .notification::<notification::DidSaveTextDocument>(Self::did_save)
            .notification::<notification::DidCloseTextDocument>(Self::did_close)
            .notification::<lsp_ext::SetSourceRoot>(Self::set_source_root)
            .notification::<lsp_ext::ClearSourceRoot>(Self::clear_source_root)
            .request_snap::<request::DocumentSymbolRequest>(handlers::document_symbol)
            .request_snap::<request::GotoDefinition>(handlers::definition)
            .request_snap::<request::References>(handlers::references)
            .request_snap::<request::HoverRequest>(handlers::hover)
            .request_snap::<request::InlayHintRequest>(handlers::inlay_hint)
            .request_snap::<request::Completion>(handlers::completion)
            .request_snap::<request::DocumentLinkRequest>(handlers::document_link)
            .request_snap::<request::FoldingRangeRequest>(handlers::folding_range)
            .event::<UpdateDiagnosticsEvent>(Self::update_diagnostics)
            .event::<UpdateConfigEvent>(Self::update_config)
            .event::<UpdateFlycheckEvent>(Self::update_flycheck);
        router
    }

    fn new(client: ClientSocket) -> Self {
        Self {
            host: AnalysisHost::new(),
            vfs: SharedFs::new(Vfs::new()),
            client,
            config: Arc::new(Config::default()),
            diagnostic_version: 0,
            opened_source_units: HashSet::new(),
            root_source_unit: None,
            flycheck_task: None,
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
    ) -> BoxFuture<'static, Result<InitializeResult>> {
        tracing::info!("initialize: {params:?}");

        // NOTE: It seems that did_change_configuration is not called, so we need to update the config here.
        if let Some(options) = params.initialization_options
            && options.as_object().filter(|it| !it.is_empty()).is_some()
        {
            let _ = self.update_config(UpdateConfigEvent(options));
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
                    trigger_characters: Some(vec![".".to_string(), "!".to_string()]),
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

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> Self::NotifyResult {
        tracing::info!("did_open: {params:?}");
        let path = UrlExt::to_file_path(&params.text_document.uri);
        let file_id = self.vfs.assign_or_get_file_id(path);
        self.host
            .set_file_content(file_id, Arc::from(params.text_document.text.as_str()));

        if !self.is_file_in_root_source_unit(&params.text_document.uri) {
            self.client
                .show_message(ShowMessageParams {
                    typ: MessageType::WARNING,
                    message: "This file is not included in the source root. Analysis is disabled."
                        .to_string(),
                })
                .expect("failed to show message");
            return ControlFlow::Continue(());
        }

        if let Some(_) = self.root_source_unit {
            // set_source_rootでこのファイルはすでに解析されているはずなので、何もしない
        } else {
            self.load_source_unit(&params.text_document.uri, &params.text_document.text);
            self.spawn_update_diagnostics();
            self.spawn_flycheck(Some(&params.text_document.uri));
        }

        ControlFlow::Continue(())
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> Self::NotifyResult {
        tracing::info!("did_change: {params:?}");
        if let Some(change) = params.content_changes.first() {
            if self.is_file_in_root_source_unit(&params.text_document.uri) {
                self.load_source_unit(&params.text_document.uri, &change.text);
                self.spawn_update_diagnostics();
            } else {
                let path = UrlExt::to_file_path(&params.text_document.uri);
                let file_id = self.vfs.assign_or_get_file_id(path);
                self.host
                    .set_file_content(file_id, Arc::from(change.text.as_str()));
            }
        }
        ControlFlow::Continue(())
    }

    fn did_save(&mut self, params: DidSaveTextDocumentParams) -> Self::NotifyResult {
        tracing::info!("did_save: {params:?}");
        if self.is_file_in_root_source_unit(&params.text_document.uri) {
            self.spawn_flycheck(Some(&params.text_document.uri));
        }
        ControlFlow::Continue(())
    }

    fn did_close(&mut self, params: DidCloseTextDocumentParams) -> Self::NotifyResult {
        tracing::info!("did_close: {params:?}");
        {
            let path = UrlExt::to_file_path(&params.text_document.uri);
            let Some(file_id) = self.vfs.file_for_path(&path) else {
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
            let Some(content) = self.vfs.read_content(&path) else {
                tracing::warn!("failed to read file: {path:?}");
                return ControlFlow::Continue(());
            };
            content
        };
        let source_unit_id = self.load_source_unit(&params.uri, &content);
        self.root_source_unit.replace(source_unit_id);
        self.spawn_update_diagnostics();
        self.spawn_flycheck(Some(&params.uri));
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
    fn snapshot(&self) -> ServerSnapshot {
        ServerSnapshot {
            analysis: self.host.analysis(),
            vfs: self.vfs.clone(),
            client: self.client.clone(),
            config: self.config.clone(),
            root_source_unit: self.root_source_unit,
        }
    }

    fn spawn_with_snapshot<P: Send + 'static, T: Send + 'static>(
        &mut self,
        params: P,
        f: impl FnOnce(ServerSnapshot, P) -> T + Send + 'static,
    ) -> task::JoinHandle<T> {
        let snap = self.snapshot();
        task::spawn_blocking(move || f(snap, params))
    }

    fn spawn_update_diagnostics(&mut self) {
        let diag_version = self.bump_diagnostic_version();
        let opened_source_units = self.opened_source_units();
        self.spawn_with_snapshot((), move |snap, _| {
            let mut lsp_diags = DiagnosticCollection::default();
            for source_unit_id in opened_source_units {
                let Ok(source_unit) = snap.analysis.source_unit(source_unit_id) else {
                    return;
                };
                lsp_diags.add_source_unit_files(&source_unit);
                let Ok(diagnostics) = snap.analysis.diagnostics(source_unit_id) else {
                    return;
                };
                let Ok(_) = lsp_diags.extend(&snap, diagnostics) else {
                    return;
                };
            }
            snap.client
                .emit(UpdateDiagnosticsEvent(diag_version, lsp_diags))
                .expect("failed to emit update diagnostics event");
        });
    }

    fn spawn_update_diagnostics_of(&mut self, source_unit_id: SourceUnitId) {
        let diag_version = self.bump_diagnostic_version();
        self.spawn_with_snapshot((), move |snap, _| {
            let mut lsp_diags = DiagnosticCollection::default();
            let Ok(source_unit) = snap.analysis.source_unit(source_unit_id) else {
                return;
            };
            lsp_diags.add_source_unit_files(&source_unit);
            let Ok(diagnostics) = snap.analysis.diagnostics(source_unit_id) else {
                return;
            };
            let Ok(_) = lsp_diags.extend(&snap, diagnostics) else {
                return;
            };
            snap.client
                .emit(UpdateDiagnosticsEvent(diag_version, lsp_diags))
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
        UpdateDiagnosticsEvent(version, lsp_diags): UpdateDiagnosticsEvent,
    ) -> <Self as LanguageServer>::NotifyResult {
        tracing::info!("update_diagnostics: {version:?}");
        for (file_id, lsp_diags_for_file) in lsp_diags {
            let file_path = self.vfs.path_for_file(&file_id);
            let file_uri = UrlExt::from_file_path(&file_path);

            let params = PublishDiagnosticsParams::new(file_uri, lsp_diags_for_file, Some(version));
            self.client
                .publish_diagnostics(params)
                .expect("failed to publish diagnostics");
        }
        ControlFlow::Continue(())
    }

    fn spawn_flycheck(&mut self, trigger_uri: Option<&Url>) {
        let file_name = trigger_uri.and_then(|uri| UrlExt::to_file_path(uri).file_name());
        let snap = self.snapshot();
        let opened_source_units = self.opened_source_units();
        let task = task::spawn(async move {
            let progress = match tokio::time::timeout(
                std::time::Duration::from_secs(1),
                Progress::new(
                    snap.client.clone(),
                    FLYCHECK_PROGRESS_TOKEN,
                    "Running flycheck",
                    file_name,
                ),
            )
            .await
            {
                Ok(p) => p,
                Err(err) => {
                    tracing::warn!("flycheck: progress create timed out: {err:?}");
                    None
                }
            };

            for source_unit_id in opened_source_units {
                let Ok(source_unit) = snap.analysis.source_unit(source_unit_id) else {
                    return;
                };
                let root_file = snap.vfs.path_for_file(&source_unit.root());

                let result = match interop::parse_source_unit_with_tblgen(
                    &root_file,
                    &snap.config.include_dirs,
                    &snap.vfs,
                ) {
                    Ok(result) => result,
                    Err(err) => {
                        tracing::warn!("failed to parse source unit: {err:?}");
                        continue;
                    }
                };

                snap.client
                    .emit(UpdateFlycheckEvent(source_unit_id, result))
                    .expect("failed to emit update flycheck event");
            }

            if let Some(progress) = progress {
                progress.done();
            }
        });
        if let Some(old) = self.flycheck_task.replace(task) {
            old.abort();
        }
    }

    fn update_flycheck(
        &mut self,
        UpdateFlycheckEvent(source_unit_id, result): UpdateFlycheckEvent,
    ) -> <Self as LanguageServer>::NotifyResult {
        tracing::info!("update_flycheck: {source_unit_id:?}");
        self.host.set_tblgen_parse_result(source_unit_id, result);
        self.spawn_update_diagnostics_of(source_unit_id);
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

    fn is_file_in_root_source_unit(&self, uri: &Url) -> bool {
        match self.root_source_unit {
            None => true,
            Some(root_id) => {
                let path = UrlExt::to_file_path(uri);
                let Some(file_id) = self.vfs.file_for_path(&path) else {
                    return false;
                };
                let Ok(source_unit) = self.host.analysis().source_unit(root_id) else {
                    return false;
                };
                source_unit.contains_file(file_id)
            }
        }
    }

    fn load_source_unit(&mut self, uri: &Url, text: &str) -> SourceUnitId {
        let path = UrlExt::to_file_path(uri);
        let file_id = self.vfs.assign_or_get_file_id(path);
        let text = Arc::from(text);
        self.host.set_file_content(file_id, text);
        let source_unit_id =
            self.host
                .load_source_unit(&mut self.vfs, file_id, &self.config.include_dirs);
        self.opened_source_units.insert(source_unit_id);
        source_unit_id
    }

    fn opened_source_units(&self) -> HashSet<SourceUnitId> {
        match self.root_source_unit {
            Some(source_unit_id) => HashSet::from([source_unit_id]),
            None => self.opened_source_units.clone(),
        }
    }
}

pub struct ServerSnapshot {
    pub analysis: Analysis,
    pub vfs: SharedFs<Vfs>,
    pub client: ClientSocket,
    pub config: Arc<Config>,
    pub root_source_unit: Option<SourceUnitId>,
}

impl ServerSnapshot {
    pub fn current_source_unit(&self, uri: &Url) -> Option<SourceUnitId> {
        match self.root_source_unit {
            Some(source_unit_id) => Some(source_unit_id),
            None => {
                let path = UrlExt::to_file_path(uri);
                let Some(file_id) = self.vfs.file_for_path(&path) else {
                    tracing::warn!("cannot find file id for {path:?}");
                    return None;
                };
                Some(SourceUnitId::from_root_file(file_id))
            }
        }
    }
}

trait RouterExt: BorrowMut<Router<Server>> {
    fn request_snap<R: request::Request>(
        &mut self,
        f: impl Fn(ServerSnapshot, R::Params) -> Cancellable<R::Result> + Send + Copy + 'static,
    ) -> &mut Self
    where
        R::Params: Send + 'static,
        R::Result: Send + 'static,
    {
        self.borrow_mut().request::<R, _>(move |this, params| {
            let task = this.spawn_with_snapshot(params, move |snap, params| f(snap, params));
            async move {
                match task.await {
                    Ok(Ok(result)) => Ok(result),
                    Ok(Err(e)) => {
                        tracing::info!("request cancelled: method={}, error={}", R::METHOD, e);
                        Err(ResponseError::new(
                            ErrorCode::SERVER_CANCELLED,
                            "request cancelled",
                        ))
                    }
                    Err(e) => {
                        tracing::warn!("request failed: method={} error={}", R::METHOD, e,);
                        Err(ResponseError::new(
                            ErrorCode::REQUEST_FAILED,
                            format!("request failed: {e}"),
                        ))
                    }
                }
            }
        });
        self
    }
}

impl RouterExt for Router<Server> {}

struct Progress {
    client: ClientSocket,
    token: ProgressToken,
}

impl Progress {
    async fn new(
        client: ClientSocket,
        token: impl Into<String>,
        title: impl Into<String>,
        message: Option<String>,
    ) -> Option<Self> {
        let token = ProgressToken::String(token.into());
        tracing::info!("start work done progress: {:?}", token);

        if let Err(err) = client
            .request::<request::WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
                token: token.clone(),
            })
            .await
        {
            tracing::warn!("failed to create work done progress: {err:?}");
            return None;
        }

        let this = Self { client, token };
        this.notify(WorkDoneProgress::Begin(WorkDoneProgressBegin {
            title: title.into(),
            cancellable: None,
            message: message,
            percentage: None,
        }));
        Some(this)
    }

    fn done(self) {
        std::mem::drop(self);
    }

    fn notify(&self, progress: WorkDoneProgress) {
        if let Err(err) = self
            .client
            .notify::<notification::Progress>(ProgressParams {
                token: self.token.clone(),
                value: ProgressParamsValue::WorkDone(progress),
            })
        {
            tracing::warn!("failed to notify progress: {err:?}");
        }
    }
}

impl Drop for Progress {
    fn drop(&mut self) {
        tracing::info!("end work done progress: {:?}", self.token);
        self.notify(WorkDoneProgress::End(WorkDoneProgressEnd { message: None }));
    }
}
