use ecow::{eco_format, EcoString};
use syntax::{
    parser::{TextRange, TextSize},
    SyntaxNode,
};

use crate::{
    file_system::{FileId, FileRange},
    handlers::diagnostics::Diagnostic,
    symbol_map::{symbol::SymbolId, variable::VariableId, SymbolMap},
};

use super::{scope::Scopes, Index, IndexDatabase};

pub struct IndexCtx<'a> {
    pub db: &'a dyn IndexDatabase,
    pub file_trace: Vec<FileId>,
    pub symbol_map: SymbolMap,
    pub diagnostics: Vec<Diagnostic>,
    pub scopes: Scopes,
    pub anonymous_def_index: u32,
}

impl<'a> IndexCtx<'a> {
    pub fn new(db: &'a dyn IndexDatabase, root_file: FileId) -> Self {
        Self {
            db,
            file_trace: vec![root_file],
            symbol_map: SymbolMap::default(),
            diagnostics: Vec::new(),
            scopes: Scopes::default(),
            anonymous_def_index: 0,
        }
    }

    pub fn current_file_id(&self) -> FileId {
        *self.file_trace.last().expect("file_trace is empty")
    }

    pub fn push_file(&mut self, file_id: FileId) {
        self.file_trace.push(file_id);
    }

    pub fn pop_file(&mut self) {
        self.file_trace.pop().expect("file_trace is empty");
    }

    pub fn resolve_id(&self, name: &EcoString) -> Option<SymbolId> {
        if let Some(symbol_id) = self.scopes.find_local(&self.symbol_map, name) {
            return Some(symbol_id);
        }
        if let Some(def_id) = self.symbol_map.find_def(name) {
            return Some(def_id.into());
        }
        None
    }

    pub fn resolve_id_in_current_scope(&self, name: &EcoString) -> Option<VariableId> {
        self.scopes.find_variable_in_current_scope(name)
    }

    /// `range`で与えられた位置に`message`をエラーとして記録する。
    /// ファイルIDは`current_file_id()`から取得する。
    pub fn error_by_textrange(&mut self, range: TextRange, message: impl Into<String>) {
        let file = self.current_file_id();
        self.diagnostics
            .push(Diagnostic::new(FileRange::new(file, range), message));
    }

    /// `range`で与えられた位置に`message`をエラーとして記録する。
    pub fn error_by_filerange(&mut self, range: FileRange, message: impl Into<String>) {
        self.diagnostics.push(Diagnostic::new(range, message));
    }

    /// `node`で与えられた位置に`message`をエラーとして記録する。
    /// `node`の範囲のうち、末尾のtrivial tokenの直前までの範囲を用いる。
    pub fn error_by_syntax(&mut self, node: &SyntaxNode, message: impl Into<String>) {
        fn find_last_not_trivia(node: &SyntaxNode) -> Option<TextSize> {
            let mut token = node.last_token()?;
            while token.kind().is_trivia() {
                token = token.prev_token()?;
            }
            Some(token.text_range().end())
        }

        let range = node.text_range();
        let end = find_last_not_trivia(node).unwrap_or(range.end());
        self.error_by_textrange(TextRange::new(range.start(), end), message);
    }

    pub fn finish(self) -> Index {
        Index {
            symbol_map: self.symbol_map,
            diagnostics: self.diagnostics,
        }
    }

    pub fn next_anonymous_def_name(&mut self) -> EcoString {
        let index = self.anonymous_def_index;
        self.anonymous_def_index += 1;
        eco_format!("anonymous_{index}")
    }
}
