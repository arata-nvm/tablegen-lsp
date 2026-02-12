use async_lsp::lsp_types::{
    CompletionParams, CompletionResponse, DocumentLink, DocumentLinkParams, DocumentSymbolParams,
    DocumentSymbolResponse, FoldingRange, FoldingRangeParams, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverParams, InlayHint, InlayHintParams, Location,
    ReferenceParams,
};
use ide::analysis::Cancellable;

use crate::{from_proto, server::ServerSnapshot, to_proto};

pub(crate) fn folding_range(
    snap: ServerSnapshot,
    params: FoldingRangeParams,
) -> Cancellable<Option<Vec<FoldingRange>>> {
    tracing::info!("folding_range: {params:?}");
    let (file_id, line_index) = from_proto::file(&snap, params.text_document)?;
    let Some(folding_ranges) = snap.analysis.folding_range(file_id)? else {
        return Ok(None);
    };
    let lsp_folding_ranges = folding_ranges
        .into_iter()
        .map(|it| to_proto::folding_range(&line_index, it))
        .collect();
    Ok(Some(lsp_folding_ranges))
}

pub(crate) fn inlay_hint(
    snap: ServerSnapshot,
    params: InlayHintParams,
) -> Cancellable<Option<Vec<InlayHint>>> {
    tracing::info!("inlay_hint: {params:?}");
    let Some(source_unit_id) = snap.current_source_unit(&params.text_document.uri) else {
        return Ok(None);
    };
    let (range, line_index) = from_proto::file_range(&snap, params.text_document, params.range)?;
    let Some(inlay_hints) = snap.analysis.inlay_hint(source_unit_id, range)? else {
        return Ok(None);
    };
    let lsp_inlay_hints = inlay_hints
        .into_iter()
        .map(|it| to_proto::inlay_hint(&line_index, it))
        .collect();
    Ok(Some(lsp_inlay_hints))
}

pub(crate) fn completion(
    snap: ServerSnapshot,
    params: CompletionParams,
) -> Cancellable<Option<CompletionResponse>> {
    tracing::info!("completion: {params:?}");
    let Some(source_unit_id) =
        snap.current_source_unit(&params.text_document_position.text_document.uri)
    else {
        return Ok(None);
    };
    let (pos, _) = from_proto::file_pos(&snap, params.text_document_position)?;
    let trigger_char = params.context.and_then(|it| it.trigger_character);
    let Some(completion_list) = snap
        .analysis
        .completion(source_unit_id, pos, trigger_char)?
    else {
        return Ok(None);
    };
    let lsp_completion_list = completion_list
        .into_iter()
        .map(to_proto::completion_item)
        .collect();
    Ok(Some(CompletionResponse::Array(lsp_completion_list)))
}

pub(crate) fn hover(snap: ServerSnapshot, params: HoverParams) -> Cancellable<Option<Hover>> {
    tracing::info!("hover: {params:?}");
    let Some(source_unit_id) =
        snap.current_source_unit(&params.text_document_position_params.text_document.uri)
    else {
        return Ok(None);
    };
    let (pos, _) = from_proto::file_pos(&snap, params.text_document_position_params)?;
    let Some(hover) = snap.analysis.hover(source_unit_id, pos)? else {
        return Ok(None);
    };
    let lsp_hover = to_proto::hover(hover);
    Ok(Some(lsp_hover))
}

pub(crate) fn definition(
    snap: ServerSnapshot,
    params: GotoDefinitionParams,
) -> Cancellable<Option<GotoDefinitionResponse>> {
    tracing::info!("goto_definition: {params:?}");
    let Some(source_unit_id) =
        snap.current_source_unit(&params.text_document_position_params.text_document.uri)
    else {
        return Ok(None);
    };
    let (pos, _) = from_proto::file_pos(&snap, params.text_document_position_params)?;
    let Some(location) = snap.analysis.goto_definition(source_unit_id, pos)? else {
        return Ok(None);
    };
    let lsp_location = to_proto::location(&snap, location)?;
    Ok(Some(GotoDefinitionResponse::Scalar(lsp_location)))
}

pub(crate) fn references(
    snap: ServerSnapshot,
    params: ReferenceParams,
) -> Cancellable<Option<Vec<Location>>> {
    tracing::info!("references: {params:?}");
    let Some(source_unit_id) =
        snap.current_source_unit(&params.text_document_position.text_document.uri)
    else {
        return Ok(None);
    };
    let (pos, _) = from_proto::file_pos(&snap, params.text_document_position)?;
    let Some(location_list) = snap.analysis.references(source_unit_id, pos)? else {
        return Ok(None);
    };
    let lsp_location_list = location_list
        .into_iter()
        .map(|it| to_proto::location(&snap, it))
        .collect::<Cancellable<_>>()?;
    Ok(Some(lsp_location_list))
}

pub(crate) fn document_symbol(
    snap: ServerSnapshot,
    params: DocumentSymbolParams,
) -> Cancellable<Option<DocumentSymbolResponse>> {
    tracing::info!("document_symbol: {params:?}");
    let (file_id, line_index) = from_proto::file(&snap, params.text_document)?;
    let Some(symbols) = snap.analysis.document_symbol(file_id)? else {
        return Ok(None);
    };
    let lsp_symbols = symbols
        .into_iter()
        .map(|it| to_proto::document_symbol(&line_index, it))
        .collect();
    Ok(Some(DocumentSymbolResponse::Nested(lsp_symbols)))
}

pub(crate) fn document_link(
    snap: ServerSnapshot,
    params: DocumentLinkParams,
) -> Cancellable<Option<Vec<DocumentLink>>> {
    tracing::info!("document link: {params:?}");
    let Some(source_unit_id) = snap.current_source_unit(&params.text_document.uri) else {
        return Ok(None);
    };
    let (file_id, line_index) = from_proto::file(&snap, params.text_document)?;
    let Some(links) = snap.analysis.document_link(source_unit_id, file_id)? else {
        return Ok(None);
    };
    let lsp_links = links
        .into_iter()
        .map(|it| to_proto::document_link(&snap.vfs, &line_index, it))
        .collect();
    Ok(Some(lsp_links))
}
