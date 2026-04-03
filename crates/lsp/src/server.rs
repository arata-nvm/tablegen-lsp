use std::borrow::BorrowMut;
use std::collections::{HashMap, HashSet};
use std::ops::ControlFlow;
use std::sync::Arc;

use async_lsp::lsp_types::{
    CompletionOptions, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentLinkOptions,
    FoldingRangeProviderCapability, HoverProviderCapability, InitializeParams, InitializeResult,
    MessageType, OneOf, PublishDiagnosticsParams, ServerCapabilities, ServerInfo,
    ShowMessageParams, SignatureHelpOptions, TextDocumentSyncCapability, TextDocumentSyncKind,
    Url, notification, request,
};
use async_lsp::router::Router;
use async_lsp::{ClientSocket, ErrorCode, LanguageClient, LanguageServer, ResponseError};
use futures::future::{BoxFuture, ready};
use ide::interop::{self, TblgenParseResult};
use serde_json::Value;
use thiserror::Error;
use tokio::task::{self};

use ide::analysis::{Analysis, AnalysisHost, Cancellable, Cancelled};
use ide::file_system::{FileId, FilePath, FileSystem, SourceUnitId};
use ide::index::Index;

use crate::config::Config;
use crate::diagnostics::DiagnosticCollection;
use crate::pending_changes::PendingChanges;
use crate::progress::Progress;
use crate::source_unit_manager::SourceUnitManager;
use crate::vfs::{SharedFs, Vfs, file_path_to_url, url_to_file_path};
use crate::{handlers, lsp_ext};

pub struct Server {
    host: AnalysisHost,
    vfs: SharedFs<Vfs>,
    client: ClientSocket,
    config: Arc<Config>,
    diagnostic_version: i32,
    source_units: SourceUnitManager,
    pending: PendingChanges,
    flycheck_task: Option<task::JoinHandle<()>>,
}

const FLYCHECK_PROGRESS_TOKEN: &str = "tablegen-lsp/flycheck";

#[derive(Error, Debug)]
enum SetSourceRootError {
    #[error("failed to read content of source root file: {0}")]
    FailedToReadContent(String),
    #[error(transparent)]
    Cancelled(#[from] Cancelled),
}

pub type Result<T> = std::result::Result<T, ResponseError>;

struct UpdateDiagnosticsEvent(i32, DiagnosticCollection);
struct UpdateConfigEvent(Value);
struct UpdateFlycheckEvent(SourceUnitId, TblgenParseResult);
struct UpdateIndexEvent(SourceUnitId, Arc<Index>);
pub struct DebounceTimerEvent;

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
            .request_snap::<request::SignatureHelpRequest>(handlers::signature_help)
            .request_snap::<request::InlayHintRequest>(handlers::inlay_hint)
            .request_snap::<request::Completion>(handlers::completion)
            .request_snap::<request::DocumentLinkRequest>(handlers::document_link)
            .request_snap::<request::FoldingRangeRequest>(handlers::folding_range)
            .event::<UpdateDiagnosticsEvent>(Self::update_diagnostics)
            .event::<UpdateConfigEvent>(Self::update_config)
            .event::<UpdateFlycheckEvent>(Self::update_flycheck)
            .event::<UpdateIndexEvent>(Self::update_index)
            .event::<DebounceTimerEvent>(Self::handle_debounce_timer);
        router
    }

    fn new(client: ClientSocket) -> Self {
        Self {
            host: AnalysisHost::new(),
            vfs: SharedFs::new(Vfs::new()),
            client,
            config: Arc::new(Config::default()),
            diagnostic_version: 0,
            source_units: SourceUnitManager::new(),
            pending: PendingChanges::new(),
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
            let result = self.client.emit(UpdateConfigEvent(options));
            if let Err(err) = result {
                tracing::warn!("failed to emit update config event: {err:?}");
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
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec![
                        "<".to_string(),
                        "(".to_string(),
                        ",".to_string(),
                    ]),
                    ..Default::default()
                }),
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
        let Ok(path) = url_to_file_path(&params.text_document.uri)
            .inspect_err(|e| tracing::warn!("did_open: {e}"))
        else {
            return ControlFlow::Continue(());
        };
        let file_name = path.file_name();
        let file_id = self.vfs.assign_or_get_file_id(path);
        self.host
            .set_file_content(file_id, Arc::from(params.text_document.text.as_str()));

        if !self.should_analyze_file(file_id) {
            if let Err(err) = self.client.show_message(ShowMessageParams {
                typ: MessageType::WARNING,
                message: "This file is not included in the source root. Analysis is disabled."
                    .to_string(),
            }) {
                tracing::warn!("failed to show message: {err:?}");
            }
            return ControlFlow::Continue(());
        }

        if self.source_units.root().is_some() {
            self.spawn_update_diagnostics();
            return ControlFlow::Continue(());
        }

        if self.load_source_unit(file_id).is_ok() {
            self.spawn_update_diagnostics();
            self.spawn_flycheck(file_name);
        }

        ControlFlow::Continue(())
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> Self::NotifyResult {
        tracing::info!("did_change: {params:?}");
        if let Some(change) = params.content_changes.first() {
            let Ok(path) = url_to_file_path(&params.text_document.uri)
                .inspect_err(|e| tracing::warn!("did_change: {e}"))
            else {
                return ControlFlow::Continue(());
            };
            let file_id = self.vfs.assign_or_get_file_id(path);
            self.pending
                .enqueue(file_id, Arc::from(change.text.as_str()));

            if self.should_analyze_file(file_id) {
                self.pending.schedule_debounce(file_id, self.client.clone());
            }
        }
        ControlFlow::Continue(())
    }

    fn did_save(&mut self, params: DidSaveTextDocumentParams) -> Self::NotifyResult {
        tracing::info!("did_save: {params:?}");
        let Ok(path) = url_to_file_path(&params.text_document.uri)
            .inspect_err(|e| tracing::warn!("did_save: {e}"))
        else {
            return ControlFlow::Continue(());
        };
        let file_name = path.file_name();
        let file_id = self.vfs.assign_or_get_file_id(path);
        if self.should_analyze_file(file_id) {
            self.spawn_flycheck(file_name);
        }
        ControlFlow::Continue(())
    }

    fn did_close(&mut self, params: DidCloseTextDocumentParams) -> Self::NotifyResult {
        tracing::info!("did_close: {params:?}");
        let Ok(path) = url_to_file_path(&params.text_document.uri)
            .inspect_err(|e| tracing::warn!("did_close: {e}"))
        else {
            return ControlFlow::Continue(());
        };
        let Some(file_id) = self.vfs.file_for_path(&path) else {
            tracing::warn!("cannot find file id: {path:?}");
            return ControlFlow::Continue(());
        };

        self.pending.cancel_for_file(file_id);

        let source_unit_id = SourceUnitId::from_root_file(file_id);
        self.source_units.close(source_unit_id);
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
        let Ok(path) =
            url_to_file_path(&params.uri).inspect_err(|e| tracing::warn!("set_source_root: {e}"))
        else {
            return ControlFlow::Continue(());
        };
        if let Err(err) = self.set_source_root_impl(path) {
            tracing::warn!("failed to set source root to {}: {}", params.uri, err);
        }
        ControlFlow::Continue(())
    }

    fn clear_source_root(
        &mut self,
        _params: lsp_ext::ClearSourceRootParams,
    ) -> <Self as LanguageServer>::NotifyResult {
        tracing::info!("clear_source_root");
        self.source_units.clear_root();
        self.spawn_update_diagnostics();
        ControlFlow::Continue(())
    }
}

impl Server {
    fn spawn_with_snapshot<P: Send + 'static, T: Send + 'static>(
        &mut self,
        params: P,
        f: impl FnOnce(ServerSnapshot, P) -> Cancellable<T> + Send + 'static,
    ) -> task::JoinHandle<Cancellable<T>> {
        let snap = ServerSnapshot {
            analysis: self.host.analysis(),
            vfs: self.vfs.clone(),
            config: self.config.clone(),
            root_source_unit: self.source_units.root(),
            latest_indexes: self.source_units.latest_indices().clone(),
        };
        task::spawn_blocking(move || f(snap, params))
    }

    fn spawn_update_diagnostics(&mut self) {
        self.spawn_update_diagnostics_for(self.source_units.active());
    }

    fn spawn_update_diagnostics_of(&mut self, source_unit_id: SourceUnitId) {
        self.spawn_update_diagnostics_for(HashSet::from([source_unit_id]));
    }

    fn spawn_update_diagnostics_for(&mut self, source_unit_ids: HashSet<SourceUnitId>) {
        let diag_version = self.bump_diagnostic_version();
        let client = self.client.clone();
        let task = self.spawn_with_snapshot(source_unit_ids, move |snap, source_unit_ids| {
            let mut lsp_diags = DiagnosticCollection::default();
            let mut indices = Vec::new();
            for source_unit_id in source_unit_ids {
                let source_unit = snap.analysis.source_unit(source_unit_id)?;
                lsp_diags.add_source_unit_files(&source_unit);
                let diagnostics = snap.analysis.diagnostics(source_unit_id)?;
                lsp_diags.extend(&snap, diagnostics)?;
                let index = snap.analysis.index(source_unit_id)?;
                indices.push((source_unit_id, index));
            }
            Ok((lsp_diags, indices))
        });
        let task = task::spawn(async move {
            let result = match task.await {
                Ok(result) => result,
                Err(e) => {
                    tracing::warn!("diagnostics task failed: {e}");
                    return;
                }
            };
            match result {
                Ok((lsp_diags, indices)) => {
                    for (source_unit_id, index) in indices {
                        if let Err(err) = client.emit(UpdateIndexEvent(source_unit_id, index)) {
                            tracing::warn!("failed to emit update index event: {err:?}");
                            return;
                        }
                    }
                    if let Err(err) = client.emit(UpdateDiagnosticsEvent(diag_version, lsp_diags)) {
                        tracing::warn!("failed to emit update diagnostics event: {err:?}");
                    }
                }
                Err(e) => tracing::info!("diagnostics cancelled: {e}"),
            }
        });
        // fire-and-forgetでバックグラウンドで実行させる
        std::mem::drop(task);
    }

    fn bump_diagnostic_version(&mut self) -> i32 {
        let version = self.diagnostic_version;
        self.diagnostic_version += 1;
        version
    }

    fn update_index(
        &mut self,
        UpdateIndexEvent(source_unit_id, index): UpdateIndexEvent,
    ) -> <Self as LanguageServer>::NotifyResult {
        tracing::info!("update_index: {source_unit_id:?}");
        self.source_units.update_index(source_unit_id, index);
        ControlFlow::Continue(())
    }

    fn update_diagnostics(
        &mut self,
        UpdateDiagnosticsEvent(version, lsp_diags): UpdateDiagnosticsEvent,
    ) -> <Self as LanguageServer>::NotifyResult {
        tracing::info!("update_diagnostics: {version:?}");
        for (file_id, lsp_diags_for_file) in lsp_diags {
            let file_path = self.vfs.path_for_file(&file_id);
            let Ok(file_uri) = file_path_to_url(&file_path)
                .inspect_err(|e| tracing::warn!("update_diagnostics: {e}"))
            else {
                continue;
            };

            let params = PublishDiagnosticsParams::new(file_uri, lsp_diags_for_file, Some(version));
            if let Err(err) = self.client.publish_diagnostics(params) {
                tracing::warn!("failed to publish diagnostics: {err:?}");
            }
        }
        ControlFlow::Continue(())
    }

    fn spawn_flycheck(&mut self, trigger_file_name: Option<String>) {
        let opened_source_units = self.source_units.active();
        let client = self.client.clone();
        let config = self.config.clone();
        let vfs = self.vfs.clone();

        let prep = self.spawn_with_snapshot(opened_source_units, |snap, ids| {
            let mut jobs = Vec::new();
            for id in ids {
                let source_unit = snap.analysis.source_unit(id)?;
                jobs.push((id, snap.vfs.path_for_file(&source_unit.root())));
            }
            Ok(jobs)
        });

        let task = task::spawn(async move {
            let result = match prep.await {
                Ok(result) => result,
                Err(e) => {
                    tracing::warn!("flycheck prep task failed: {e}");
                    return;
                }
            };
            let jobs = match result {
                Ok(jobs) => jobs,
                Err(e) => {
                    tracing::info!("flycheck prep cancelled: {e}");
                    return;
                }
            };

            let progress = Progress::new(
                client.clone(),
                FLYCHECK_PROGRESS_TOKEN,
                "Running flycheck",
                trigger_file_name,
            )
            .await;

            for (source_unit_id, root_file) in jobs {
                let result = match interop::parse_source_unit_with_tblgen(
                    &root_file,
                    &config.include_dirs,
                    &vfs,
                ) {
                    Ok(result) => result,
                    Err(err) => {
                        tracing::warn!("failed to parse source unit: {err:?}");
                        continue;
                    }
                };

                if let Err(err) = client.emit(UpdateFlycheckEvent(source_unit_id, result)) {
                    tracing::warn!("failed to emit update flycheck event: {err:?}");
                }
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
            return ControlFlow::Continue(());
        }

        if let Some(source_root_path) = config.default_source_root_path.clone()
            && let Err(err) = self.set_source_root_impl(source_root_path)
        {
            let message = format!("tablegen-lsp: failed to set source root: {err}");
            tracing::warn!("{message}");
            if let Err(err) = self.client.show_message(ShowMessageParams {
                typ: MessageType::ERROR,
                message,
            }) {
                tracing::warn!("failed to show message: {err:?}");
            }
        }

        ControlFlow::Continue(())
    }

    fn should_analyze_file(&self, file_id: FileId) -> bool {
        match self.source_units.root() {
            None => true,
            Some(root_id) => {
                let Ok(source_unit) = self.host.analysis().source_unit(root_id) else {
                    return false;
                };
                source_unit.contains_file(file_id)
            }
        }
    }

    fn load_source_unit(&mut self, file_id: FileId) -> Cancellable<SourceUnitId> {
        let source_unit_id =
            self.host
                .load_source_unit(&mut self.vfs, file_id, &self.config.include_dirs)?;
        self.source_units.open(source_unit_id);
        Ok(source_unit_id)
    }

    fn handle_debounce_timer(
        &mut self,
        _: DebounceTimerEvent,
    ) -> <Self as LanguageServer>::NotifyResult {
        self.pending.apply_to(&mut self.host);

        if let Some(file_id) = self.pending.take_debounce_file_id() {
            match self
                .host
                .load_source_unit(&mut self.vfs, file_id, &self.config.include_dirs)
            {
                Ok(source_unit_id) => {
                    self.source_units.open(source_unit_id);
                    self.spawn_update_diagnostics();
                }
                Err(e) => {
                    tracing::warn!("debounced load_source_unit cancelled: {e:?}");
                }
            }
        }
        ControlFlow::Continue(())
    }

    fn set_source_root_impl(
        &mut self,
        path: FilePath,
    ) -> std::result::Result<(), SetSourceRootError> {
        let content = self
            .vfs
            .read_content(&path)
            .ok_or_else(|| SetSourceRootError::FailedToReadContent(path.to_str().to_string()))?;

        let file_name = path.file_name();
        let file_id = self.vfs.assign_or_get_file_id(path);
        self.host
            .set_file_content(file_id, Arc::from(content.as_str()));
        let source_unit_id = self.load_source_unit(file_id)?;
        self.source_units.set_root(source_unit_id);
        self.spawn_update_diagnostics();
        self.spawn_flycheck(file_name);
        Ok(())
    }
}

pub struct ServerSnapshot {
    pub analysis: Analysis,
    pub vfs: SharedFs<Vfs>,
    pub config: Arc<Config>,
    pub root_source_unit: Option<SourceUnitId>,
    pub latest_indexes: HashMap<SourceUnitId, Arc<Index>>,
}

impl ServerSnapshot {
    pub fn current_source_unit(&self, uri: &Url) -> Option<SourceUnitId> {
        match self.root_source_unit {
            Some(source_unit_id) => Some(source_unit_id),
            None => {
                let Ok(path) = url_to_file_path(uri)
                    .inspect_err(|e| tracing::warn!("current_source_unit: {e}"))
                else {
                    return None;
                };
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
            this.pending.apply_to(&mut this.host);
            let task = this.spawn_with_snapshot(params, f);
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
