use ecow::EcoString;
use id_arena::Arena;
use iset::IntervalMap;
use tablegen_parser::error::Position;

use crate::symbol::{self, SymbolType};

use super::symbol::{Location, OldSymbol, OldSymbolId, SymbolKind};

#[derive(Debug)]
pub struct SymbolMap {
    symbols: Arena<OldSymbol>,
    symbol_map: IntervalMap<Position, OldSymbolId>,
    records: Vec<OldSymbolId>,
}

impl SymbolMap {
    pub fn new() -> Self {
        Self {
            symbols: Arena::new(),
            symbol_map: IntervalMap::new(),
            records: Vec::new(),
        }
    }

    pub fn new_symbol(
        &mut self,
        name: EcoString,
        kind: SymbolKind,
        define_loc: Location,
        typ: SymbolType,
    ) -> OldSymbolId {
        let symbol = OldSymbol::new(name, kind, define_loc.clone(), typ);
        let symbol_id = self.symbols.alloc(symbol);
        self.symbol_map.insert(define_loc.1, symbol_id);

        if let SymbolKind::Record = kind {
            self.records.push(symbol_id);
        }

        symbol_id
    }

    pub fn symbol(&self, symbol_id: OldSymbolId) -> Option<&OldSymbol> {
        self.symbols.get(symbol_id)
    }

    pub fn symbol_mut(&mut self, symbol_id: OldSymbolId) -> Option<&mut OldSymbol> {
        self.symbols.get_mut(symbol_id)
    }

    pub fn add_reference(&mut self, symbol_id: OldSymbolId, loc: Location) {
        let symbol = self.symbol_mut(symbol_id).unwrap();
        symbol.add_reference(loc.clone());
        self.symbol_map.insert(loc.1, symbol_id);
    }

    pub fn get_symbol_at(&self, pos: Position) -> Option<&OldSymbol> {
        self.symbol_map
            .values_overlap(pos)
            .next()
            .and_then(|&id| self.symbols.get(id))
    }

    pub fn records(&self) -> &[OldSymbolId] {
        &self.records
    }
}
