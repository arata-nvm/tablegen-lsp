use ecow::EcoString;
use id_arena::Arena;
use iset::IntervalMap;

use tablegen_parser::parser::TextSize;

use crate::symbol::{
    Location, Record, RecordField, RecordFieldKind, RecordKind, Symbol, SymbolId, SymbolType,
    Variable, VariableKind,
};

#[derive(Debug)]
pub struct SymbolMap {
    symbols: Arena<Symbol>,
    symbol_map: IntervalMap<TextSize, SymbolId>,
    global_symbols: Vec<SymbolId>,
}

impl SymbolMap {
    pub fn new() -> Self {
        Self {
            symbols: Arena::new(),
            symbol_map: IntervalMap::new(),
            global_symbols: Vec::new(),
        }
    }

    pub fn new_record(
        &mut self,
        name: EcoString,
        define_loc: Location,
        kind: RecordKind,
    ) -> SymbolId {
        let record = Record::new(name, define_loc.clone(), kind);
        let symbol_id = self.symbols.alloc(Symbol::Record(record));
        self.symbol_map.insert(define_loc.1.into(), symbol_id);
        self.global_symbols.push(symbol_id);
        symbol_id
    }

    pub fn new_record_field(
        &mut self,
        name: EcoString,
        define_loc: Location,
        kind: RecordFieldKind,
        typ: SymbolType,
    ) -> SymbolId {
        let field = RecordField::new(name, define_loc.clone(), kind, typ);
        let symbol_id = self.symbols.alloc(Symbol::RecordField(field));
        self.symbol_map.insert(define_loc.1.into(), symbol_id);
        symbol_id
    }

    pub fn new_variable(
        &mut self,
        name: EcoString,
        define_loc: Location,
        kind: VariableKind,
        typ: SymbolType,
    ) -> SymbolId {
        let variable = Variable::new(name, define_loc.clone(), kind, typ);
        let symbol_id = self.symbols.alloc(Symbol::Variable(variable));
        self.symbol_map.insert(define_loc.1.into(), symbol_id);
        self.global_symbols.push(symbol_id);
        symbol_id
    }

    pub fn new_temporary_variable(
        &mut self,
        name: EcoString,
        define_loc: Location,
        kind: VariableKind,
        typ: SymbolType,
    ) -> SymbolId {
        let variable = Variable::new(name, define_loc.clone(), kind, typ);
        let symbol_id = self.symbols.alloc(Symbol::Variable(variable));
        self.symbol_map.insert(define_loc.1.into(), symbol_id);
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
        self.symbol_map.insert(loc.1.into(), symbol_id);
    }

    pub fn get_symbol_at(&self, pos: TextSize) -> Option<&Symbol> {
        self.symbol_map
            .values_overlap(pos)
            .next()
            .and_then(|&id| self.symbols.get(id))
    }

    pub fn global_symbols(&self) -> &[SymbolId] {
        &self.global_symbols
    }

    pub fn find_field(&self, symbol_id: SymbolId, name: EcoString) -> Option<SymbolId> {
        let record = self.symbol(symbol_id)?.as_record()?;
        if let Some(field_id) = record.find_field(&name).cloned() {
            return Some(field_id);
        }

        let parents = record.parents().to_vec();
        for parent_symbol_id in parents.into_iter().rev() {
            if let Some(field_id) = self.find_field(parent_symbol_id, name.clone()) {
                return Some(field_id);
            }
        }

        None
    }

    pub fn get_all_fields(&self, symbol: &Symbol) -> Vec<SymbolId> {
        let Some(record) = symbol.as_record() else {
            return vec![];
        };

        let mut fields = vec![];
        fields.extend(record.fields());
        for parent_id in record.parents() {
            let Some(parent) = self.symbol(*parent_id) else {
                continue;
            };
            fields.extend(self.get_all_fields(parent));
        }
        fields
    }
}
