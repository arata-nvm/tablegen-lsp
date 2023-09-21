use ecow::EcoString;
use id_arena::Arena;
use iset::IntervalMap;
use tablegen_parser::{ast, error::Span, node::SyntaxNode};
use tower_lsp::lsp_types::Url;

use super::symbol::{TableGenSymbol, TableGenSymbolId};

#[derive(Debug)]
pub struct TableGenDocumentIndex {
    uri: Url,
    symbols: Arena<TableGenSymbol>,
    symbol_map: IntervalMap<usize, TableGenSymbolId>,
}

impl TableGenDocumentIndex {
    pub fn get_symbol_at(&self, loc: usize) -> Option<&TableGenSymbol> {
        self.symbol_map
            .values_overlap(loc)
            .next()
            .and_then(|&id| self.symbols.get(id))
    }
}

impl TableGenDocumentIndex {
    fn new(uri: Url) -> Self {
        Self {
            uri,
            symbols: Arena::new(),
            symbol_map: IntervalMap::new(),
        }
    }

    // TODO: wrap SyntaxNode
    pub fn create_index(uri: Url, file: &SyntaxNode) -> Self {
        let mut index = Self::new(uri);
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
        self.add_symbol(name.value()?, name.span());
        None
    }

    fn add_symbol(&mut self, name: &EcoString, span: Span) {
        let symbol = TableGenSymbol::new(name.clone(), self.uri.clone(), span.clone());
        let symbol_id = self.symbols.alloc(symbol);
        self.symbol_map.insert(span, symbol_id);
    }
}
