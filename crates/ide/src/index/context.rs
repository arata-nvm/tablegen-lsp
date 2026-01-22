use std::collections::HashMap;
use std::sync::Arc;

use ecow::{EcoString, eco_format};
use syntax::{
    SyntaxNode, SyntaxNodePtr,
    parser::{TextRange, TextSize},
};

use crate::{
    file_system::{FileId, FilePosition, FileRange, SourceUnit},
    handlers::diagnostics::Diagnostic,
    interop::{TblgenDef, TblgenSymbolTable},
    symbol_map::{SymbolMap, def::DefId, symbol::SymbolId, typ::Type, variable::VariableId},
};

use super::{Index, IndexDatabase, scope::Scopes};

pub struct IndexCtx<'a> {
    pub db: &'a dyn IndexDatabase,
    pub source_unit: Arc<SourceUnit>,
    pub file_trace: Vec<FileId>,
    pub symbol_map: SymbolMap,
    pub diagnostics: Vec<Diagnostic>,
    pub scopes: Scopes,
    pub anonymous_def_index: u32,
    pub tblgen_symtab: Arc<TblgenSymbolTable>,
    pub pos_to_multiclass_def_map: HashMap<FilePosition, DefId>,
    pub resolved_types: HashMap<SyntaxNodePtr, Type>,
}

impl<'a> IndexCtx<'a> {
    pub fn new(
        db: &'a dyn IndexDatabase,
        source_unit: Arc<SourceUnit>,
        tblgen_symtab: Arc<TblgenSymbolTable>,
    ) -> Self {
        let root = source_unit.root();
        Self {
            db,
            source_unit,
            file_trace: vec![root],
            symbol_map: SymbolMap::default(),
            diagnostics: Vec::new(),
            scopes: Scopes::default(),
            anonymous_def_index: 0,
            tblgen_symtab,
            pos_to_multiclass_def_map: HashMap::new(),
            resolved_types: HashMap::new(),
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
        if let Some(defset_id) = self.symbol_map.find_defset(name) {
            return Some(defset_id.into());
        }
        None
    }

    pub fn resolve_id_in_current_scope(&self, name: &EcoString) -> Option<VariableId> {
        self.scopes.find_variable_in_current_scope(name)
    }

    pub fn get_tblgen_defs_at(&self, file_pos: &FilePosition) -> &[TblgenDef] {
        self.tblgen_symtab.get_defs_at(file_pos)
    }

    pub fn add_multiclass_def(&mut self, def_kw_pos: FilePosition, def_id: DefId) {
        self.pos_to_multiclass_def_map.insert(def_kw_pos, def_id);
    }

    pub fn get_multiclass_def_at(&self, def_kw_pos: &FilePosition) -> Option<DefId> {
        self.pos_to_multiclass_def_map.get(def_kw_pos).copied()
    }

    /// `range`で与えられた位置に`message`をエラーとして記録する。
    /// ファイルIDは`current_file_id()`から取得する。
    pub fn error_by_textrange(&mut self, range: TextRange, message: impl Into<String>) {
        let file = self.current_file_id();
        self.diagnostics
            .push(Diagnostic::new_lsp(FileRange::new(file, range), message));
    }

    /// `range`で与えられた位置に`message`をエラーとして記録する。
    pub fn error_by_filerange(&mut self, range: FileRange, message: impl Into<String>) {
        self.diagnostics.push(Diagnostic::new_lsp(range, message));
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
            resolved_types: self.resolved_types,
        }
    }

    pub fn next_anonymous_def_name(&mut self) -> EcoString {
        let index = self.anonymous_def_index;
        self.anonymous_def_index += 1;
        eco_format!("anonymous_{index}")
    }
}
