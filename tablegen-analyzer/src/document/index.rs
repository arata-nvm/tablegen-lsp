use ecow::EcoString;
use id_arena::Arena;
use iset::IntervalMap;
use tablegen_parser::{
    ast,
    error::{Position, Range},
    node::SyntaxNode,
};

use super::{
    symbol::{TableGenSymbol, TableGenSymbolId, TableGenSymbolKind},
    TableGenDocumentId,
};

#[derive(Debug)]
pub struct TableGenDocumentIndex {
    doc_id: TableGenDocumentId,
    symbols: Arena<TableGenSymbol>,
    symbol_map: IntervalMap<Position, TableGenSymbolId>,
}

impl TableGenDocumentIndex {
    pub fn get_symbol_at(&self, pos: Position) -> Option<&TableGenSymbol> {
        self.symbol_map
            .values_overlap(pos)
            .next()
            .and_then(|&id| self.symbols.get(id))
    }
}

impl TableGenDocumentIndex {
    fn new(doc_id: TableGenDocumentId) -> Self {
        Self {
            doc_id,
            symbols: Arena::new(),
            symbol_map: IntervalMap::new(),
        }
    }

    // TODO: wrap SyntaxNode
    pub fn create_index(doc_id: TableGenDocumentId, file: &SyntaxNode) -> Self {
        let mut index = Self::new(doc_id);
        index.analyze_file(file);
        index
    }

    fn analyze_file(&mut self, file: &SyntaxNode) -> Option<()> {
        let file = file.cast::<ast::File>()?;
        let list = file.statement_list()?;
        for stmt in list.statements() {
            match stmt {
                ast::Statement::Class(class) => {
                    let _ = self.analyze_class(class);
                }
                _ => {}
            }
        }
        None
    }

    fn analyze_class(&mut self, class: ast::Class) -> Option<()> {
        let name = class.name()?;
        self.add_symbol(name.value()?, name.range(), TableGenSymbolKind::Class);
        None
    }

    fn add_symbol(&mut self, name: &EcoString, span: Range, kind: TableGenSymbolKind) {
        let define_loc = (self.doc_id, span.clone());
        let symbol = TableGenSymbol::new(name.clone(), kind, define_loc);
        let symbol_id = self.symbols.alloc(symbol);
        self.symbol_map.insert(span, symbol_id);
    }
}
