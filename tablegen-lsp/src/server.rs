use std::{
    future::{ready, Future},
    ops::ControlFlow,
};

use async_lsp::{router::Router, ClientSocket, ResponseError};
use lsp_types::{
    notification, request, CompletionOptions, CompletionParams, CompletionResponse,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DocumentSymbolParams,
    DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InlayHint, InlayHintParams,
    Location, OneOf, ReferenceParams, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url,
};

use tablegen_analyzer::document::Document;

use crate::{
    compat::{analyzer2lsp, lsp2analyzer},
    document_map::DocumentMap,
};

mod internal;

pub struct TableGenLanguageServer {
    client: ClientSocket,
    document_map: DocumentMap,
}

impl TableGenLanguageServer {
    pub fn new_router(client: ClientSocket) -> Router<Self> {
        let this = Self::new(client);
        let mut router = Router::new(this);
        router
            .notification::<notification::Initialized>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidOpenTextDocument>(Self::did_open)
            .notification::<notification::DidChangeTextDocument>(Self::did_change)
            .notification::<notification::DidCloseTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidSaveTextDocument>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::Exit>(|_, _| ControlFlow::Continue(()))
            .request::<request::Initialize, _>(Self::initialize)
            .request::<request::Shutdown, _>(|_, _| ready(Ok(())))
            .request::<request::GotoDefinition, _>(Self::goto_definition)
            .request::<request::References, _>(Self::references)
            .request::<request::DocumentSymbolRequest, _>(Self::document_symbol)
            .request::<request::HoverRequest, _>(Self::hover)
            .request::<request::Completion, _>(Self::completion)
            .request::<request::InlayHintRequest, _>(Self::inlay_hint);
        router
    }

    fn new(client: ClientSocket) -> Self {
        Self {
            client,
            document_map: DocumentMap::new(),
        }
    }
}

type NotifyResult = ControlFlow<async_lsp::Result<()>>;

impl TableGenLanguageServer {
    fn initialize(
        &mut self,
        _params: InitializeParams,
    ) -> impl Future<Output = Result<InitializeResult, ResponseError>> {
        ready(Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec!["!".into()]),
                    ..Default::default()
                }),
                inlay_hint_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
        }))
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> NotifyResult {
        self.check_file(
            params.text_document.uri,
            params.text_document.version,
            params.text_document.text,
        );
        ControlFlow::Continue(())
    }

    fn did_change(&mut self, mut params: DidChangeTextDocumentParams) -> NotifyResult {
        self.check_file(
            params.text_document.uri,
            params.text_document.version,
            std::mem::take(&mut params.content_changes[0].text),
        );
        ControlFlow::Continue(())
    }

    fn goto_definition(
        &mut self,
        params: GotoDefinitionParams,
    ) -> impl Future<Output = Result<Option<GotoDefinitionResponse>, ResponseError>> {
        let uri = params.text_document_position_params.text_document.uri;
        let definition = self.with_document(uri, |doc_map, doc| {
            let lsp_position = params.text_document_position_params.position;
            let position = lsp2analyzer::position(doc, lsp_position);
            let definition = doc.get_definition(position)?;
            let lsp_definition = analyzer2lsp::location(doc_map, doc, definition);
            Some(GotoDefinitionResponse::Scalar(lsp_definition))
        });
        ready(Ok(definition))
    }

    fn references(
        &mut self,
        params: ReferenceParams,
    ) -> impl Future<Output = Result<Option<Vec<Location>>, ResponseError>> {
        let uri = params.text_document_position.text_document.uri;
        let references = self.with_document(uri, |doc_map, doc| {
            let lsp_position = params.text_document_position.position;
            let position = lsp2analyzer::position(doc, lsp_position);
            let references = doc.get_references(position)?;
            let lsp_references = references
                .into_iter()
                .map(|reference| analyzer2lsp::location(doc_map, doc, reference))
                .collect();
            Some(lsp_references)
        });
        ready(Ok(references))
    }

    fn document_symbol(
        &mut self,
        params: DocumentSymbolParams,
    ) -> impl Future<Output = Result<Option<DocumentSymbolResponse>, ResponseError>> {
        let uri = params.text_document.uri;
        let symbols = self.with_document(uri, |_, doc| {
            let symbols = doc.symbol_map().global_symbols();
            let lsp_symbols = symbols
                .iter()
                .filter_map(|symbol_id| doc.symbol_map().symbol(*symbol_id))
                .map(|symbol| analyzer2lsp::document_symbol(doc, symbol))
                .collect();
            Some(DocumentSymbolResponse::Nested(lsp_symbols))
        });
        ready(Ok(symbols))
    }

    fn hover(
        &mut self,
        params: HoverParams,
    ) -> impl Future<Output = Result<Option<Hover>, ResponseError>> {
        let uri = params.text_document_position_params.text_document.uri;
        let hover = self.with_document(uri, |_, doc| {
            let lsp_position = params.text_document_position_params.position;
            let position = lsp2analyzer::position(doc, lsp_position);
            let hover = doc.get_hover(position)?;
            let lsp_hover = analyzer2lsp::hover(hover);
            Some(lsp_hover)
        });
        ready(Ok(hover))
    }

    fn completion(
        &mut self,
        params: CompletionParams,
    ) -> impl Future<Output = Result<Option<CompletionResponse>, ResponseError>> {
        let uri = params.text_document_position.text_document.uri;
        let completion = self.with_document(uri, |_, doc| {
            let lsp_position = params.text_document_position.position;
            let position = lsp2analyzer::position(doc, lsp_position);
            let completion_items = doc.get_completion(position)?;
            let lsp_completion = completion_items
                .into_iter()
                .map(analyzer2lsp::completion_item)
                .collect();
            Some(CompletionResponse::Array(lsp_completion))
        });
        ready(Ok(completion))
    }

    fn inlay_hint(
        &mut self,
        params: InlayHintParams,
    ) -> impl Future<Output = Result<Option<Vec<InlayHint>>, ResponseError>> {
        let uri = params.text_document.uri;
        let inlay_hint = self.with_document(uri, |_, doc| {
            let lsp_range = params.range;
            let range = lsp2analyzer::range(doc, lsp_range);
            let inlay_hint = doc.get_inlay_hint(range)?;
            let lsp_inlay_hint = inlay_hint
                .into_iter()
                .map(|hint| analyzer2lsp::inlay_hint(doc, hint))
                .collect();
            Some(lsp_inlay_hint)
        });
        ready(Ok(inlay_hint))
    }
}

impl TableGenLanguageServer {
    fn with_document<T>(
        &self,
        uri: Url,
        f: impl FnOnce(&DocumentMap, &Document) -> Option<T>,
    ) -> Option<T> {
        let doc_id = self.document_map.to_document_id(&uri)?;
        let document = self.document_map.find_document(doc_id)?;
        f(&self.document_map, document)
    }
}
