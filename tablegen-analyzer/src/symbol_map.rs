use ecow::EcoString;
use id_arena::Arena;
use iset::IntervalMap;
use tablegen_parser::error::Position;

use crate::symbol::{Record, RecordField, RecordFieldType, Symbol, SymbolId};

use super::symbol::Location;

#[derive(Debug)]
pub struct SymbolMap {
    symbols: Arena<Symbol>,
    symbol_map: IntervalMap<Position, SymbolId>,
    records: Vec<SymbolId>,
}

impl SymbolMap {
    pub fn new() -> Self {
        Self {
            symbols: Arena::new(),
            symbol_map: IntervalMap::new(),
            records: Vec::new(),
        }
    }

    pub fn new_record(&mut self, name: EcoString, define_loc: Location) -> SymbolId {
        let record = Record::new(name, define_loc.clone());
        let symbol_id = self.symbols.alloc(Symbol::Record(record));
        self.symbol_map.insert(define_loc.1, symbol_id);
        self.records.push(symbol_id);
        symbol_id
    }

    pub fn new_record_field(
        &mut self,
        name: EcoString,
        define_loc: Location,
        typ: RecordFieldType,
    ) -> SymbolId {
        let field = RecordField::new(name, define_loc.clone(), typ);
        let symbol_id = self.symbols.alloc(Symbol::RecordField(field));
        self.symbol_map.insert(define_loc.1, symbol_id);
        symbol_id
    }

    pub fn symbol(&self, symbol_id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(symbol_id)
    }

    pub fn symbol_mut(&mut self, symbol_id: SymbolId) -> Option<&mut Symbol> {
        self.symbols.get_mut(symbol_id)
    }

    pub fn add_reference(&mut self, symbol_id: SymbolId, loc: Location) {
        let symbol = self.symbol_mut(symbol_id).unwrap();
        symbol.add_reference(loc.clone());
        self.symbol_map.insert(loc.1, symbol_id);
    }

    pub fn get_symbol_at(&self, pos: Position) -> Option<&Symbol> {
        self.symbol_map
            .values_overlap(pos)
            .next()
            .and_then(|&id| self.symbols.get(id))
    }

    pub fn records(&self) -> &[SymbolId] {
        &self.records
    }
}
