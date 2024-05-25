use ecow::EcoString;

use crate::symbol_map::{class::ClassId, def::DefId, record::Record, symbol::SymbolId};

#[derive(Debug, Default)]
pub struct Scopes {
    scopes: Vec<Scope>,
}

impl Scopes {
    pub fn push(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    pub fn pop(&mut self) -> Scope {
        self.scopes.pop().expect("scope is empty")
    }

    pub fn current_record_id(&self) -> SymbolId {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.id())
            .expect("scope is empty")
    }

    pub fn current_record(&self) -> &Record {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.record())
            .expect("scope is empty")
    }

    pub fn current_record_mut(&mut self) -> &mut Record {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|record| record.record_mut())
            .expect("scope is empty")
    }

    pub fn find_local_var(&self, name: &EcoString) -> Option<SymbolId> {
        // TODO
        if self.scopes.is_empty() {
            return None;
        }

        let record = self.current_record();

        if let Some(field_id) = record.find_field(name) {
            return Some(field_id.into());
        }

        if let Some(template_arg_id) = record.find_template_arg(name) {
            return Some(template_arg_id.into());
        }

        None
    }
}

#[derive(Debug)]
pub enum Scope {
    Class(ClassId, Record),
    Def(DefId, Record),
}

impl Scope {
    pub fn id(&self) -> Option<SymbolId> {
        match self {
            Scope::Class(id, _) => Some((*id).into()),
            Scope::Def(id, _) => Some((*id).into()),
        }
    }

    pub fn record(&self) -> Option<&Record> {
        match self {
            Scope::Class(_, record) => Some(record),
            Scope::Def(_, record) => Some(record),
        }
    }

    pub fn record_mut(&mut self) -> Option<&mut Record> {
        match self {
            Scope::Class(_, record) => Some(record),
            Scope::Def(_, record) => Some(record),
        }
    }

    pub fn into_class(self) -> (ClassId, Record) {
        match self {
            Scope::Class(id, class) => (id, class),
            _ => panic!("not a class"),
        }
    }

    pub fn into_def(self) -> (DefId, Record) {
        match self {
            Scope::Def(id, def) => (id, def),
            _ => panic!("not a def"),
        }
    }
}
