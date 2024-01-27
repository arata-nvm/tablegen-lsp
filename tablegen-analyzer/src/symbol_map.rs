use std::collections::VecDeque;
use std::ops::Range;

use ecow::{eco_format, EcoString};
use id_arena::Arena;
use iset::IntervalMap;

use tablegen_parser::parser::{TextRange, TextSize};

use crate::document::DocumentId;
use crate::symbol::{
    Location, Record, RecordField, RecordFieldKind, RecordKind, Symbol, SymbolId, SymbolType,
    Variable, VariableKind,
};

#[derive(Debug)]
pub struct SymbolMap {
    doc_id: DocumentId,
    symbols: Arena<Symbol>,
    symbol_map: IntervalMap<TextSize, SymbolId>,
    global_symbols: Vec<SymbolId>,
    anonymous_index: usize,
}

impl SymbolMap {
    pub fn new(doc_id: DocumentId) -> Self {
        Self {
            doc_id,
            symbols: Arena::new(),
            symbol_map: IntervalMap::new(),
            global_symbols: Vec::new(),
            anonymous_index: 0,
        }
    }

    pub fn new_record(
        &mut self,
        name: EcoString,
        define_loc: Location,
        kind: RecordKind,
    ) -> SymbolId {
        let record = Record::new(name, define_loc, kind);
        let symbol_id = self.symbols.alloc(Symbol::Record(record));
        self.symbol_map.insert(define_loc.1.into(), symbol_id);
        self.global_symbols.push(symbol_id);
        symbol_id
    }

    pub fn new_anonymous_record(&mut self, define_loc: Location) -> SymbolId {
        let name = self.next_anonymous_name();
        let record = Record::new(name, define_loc, RecordKind::Def);
        self.symbols.alloc(Symbol::Record(record))
    }

    fn next_anonymous_name(&mut self) -> EcoString {
        self.anonymous_index += 1;
        eco_format!("anonymous_{}", self.anonymous_index)
    }

    pub fn new_record_field(
        &mut self,
        name: EcoString,
        define_loc: Location,
        kind: RecordFieldKind,
        typ: SymbolType,
    ) -> SymbolId {
        let field = RecordField::new(name, define_loc, kind, typ);
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
        let variable = Variable::new(name, define_loc, kind, typ);
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
        let variable = Variable::new(name, define_loc, kind, typ);
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
        symbol.add_reference(loc);
        self.symbol_map.insert(loc.1.into(), symbol_id);
    }

    // doc_idが指すDocumentで定義されたシンボルのみを扱う
    pub fn get_symbol_at(&self, pos: TextSize) -> Option<&Symbol> {
        self.symbol_map
            .values_overlap(pos)
            .filter_map(|symbol_id| self.symbols.get(*symbol_id))
            .find(|symbol| symbol.define_loc().0 == self.doc_id)
    }

    // doc_idが指すDocumentで定義されたシンボルのみを扱う
    pub fn get_symbols_in(
        &self,
        range: TextRange,
    ) -> Option<impl Iterator<Item = (Range<TextSize>, &Symbol)>> {
        if range.is_empty() {
            return None;
        }

        let iter = self
            .symbol_map
            .iter::<Range<TextSize>>(range.into())
            .filter_map(|(range, symbol_id)| {
                self.symbols.get(*symbol_id).map(|symbol| (range, symbol))
            })
            .filter(|(_, symbol)| symbol.define_loc().0 == self.doc_id);
        Some(iter)
    }

    // doc_idが指すDocumentで定義されたシンボルのみを扱う
    pub fn global_symbols(&self) -> impl Iterator<Item = &Symbol> {
        self.global_symbols
            .iter()
            .filter_map(|symbol_id| self.symbols.get(*symbol_id))
            .filter(|symbol| symbol.define_loc().0 == self.doc_id)
    }

    pub fn find_field(&self, symbol_id: SymbolId, name: EcoString) -> Option<SymbolId> {
        let record = self.symbol(symbol_id)?.as_record()?;
        if let Some(field_id) = record.find_field(&name).cloned() {
            return Some(field_id);
        }

        let parents = record.parents().to_vec();
        for parent_symbol_id in parents.into_iter().rev() {
            // FIXME: 2つ以上のシンボルがまたがっているとcyclicな依存関係を見逃す場合がある
            if parent_symbol_id == symbol_id {
                break;
            }
            if let Some(field_id) = self.find_field(parent_symbol_id, name.clone()) {
                return Some(field_id);
            }
        }

        None
    }

    pub fn get_all_parent_fields(&self, symbol: &Symbol) -> Vec<SymbolId> {
        let Some(record) = symbol.as_record() else {
            return vec![];
        };

        let mut fields = vec![];

        let mut parents = VecDeque::new();
        parents.extend(record.parents());
        while let Some(parent_id) = parents.pop_front() {
            let Some(parent) = self.symbol(parent_id) else {
                continue;
            };
            let Some(record) = parent.as_record() else {
                continue;
            };

            parents.extend(record.parents());
            fields.extend(record.fields());
        }

        fields
    }
}
