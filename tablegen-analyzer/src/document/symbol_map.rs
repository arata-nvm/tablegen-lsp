use ecow::EcoString;
use id_arena::Arena;
use iset::IntervalMap;
use tablegen_parser::error::{Position, Range};

use super::symbol::{Location, Symbol, SymbolId, SymbolKind};

#[derive(Debug)]
pub struct SymbolMap {
    symbols: Arena<Symbol>,
    symbol_map: IntervalMap<Position, SymbolId>,
}

impl SymbolMap {
    pub fn new() -> Self {
        Self {
            symbols: Arena::new(),
            symbol_map: IntervalMap::new(),
        }
    }

    pub fn new_symbol(
        &mut self,
        name: EcoString,
        kind: SymbolKind,
        define_loc: Location,
    ) -> SymbolId {
        let symbol = Symbol::new(name, kind, define_loc.clone());
        let symbol_id = self.symbols.alloc(symbol);
        self.symbol_map.insert(define_loc.1, symbol_id);
        symbol_id
    }

    pub fn symbol(&self, symbol_id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(symbol_id)
    }

    pub fn symbol_mut(&mut self, symbol_id: SymbolId) -> Option<&mut Symbol> {
        self.symbols.get_mut(symbol_id)
    }

    pub fn add_reference(&mut self, symbol_id: SymbolId, range: Range) {
        self.symbol_map.insert(range, symbol_id);
    }

    pub fn get_symbol_at(&self, pos: Position) -> Option<&Symbol> {
        self.symbol_map
            .values_overlap(pos)
            .next()
            .and_then(|&id| self.symbols.get(id))
    }
}
