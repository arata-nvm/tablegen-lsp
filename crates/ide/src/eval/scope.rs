use std::collections::HashMap;

use ecow::EcoString;

use crate::symbol_map::{
    class::ClassId,
    def::DefId,
    record::Record,
    symbol::SymbolId,
    variable::{Variable, VariableId},
    SymbolMap,
};

#[derive(Debug)]
pub struct Scopes {
    scopes: Vec<Scope>,
}

impl Default for Scopes {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::new(ScopeKind::Root)],
        }
    }
}

impl Scopes {
    pub fn push(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope::new(kind));
    }

    pub fn pop(&mut self) -> Scope {
        self.scopes.pop().expect("scope is empty")
    }

    pub fn current_record_id(&self) -> SymbolId {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.record_id())
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

    pub fn add_variable(&mut self, symbol_map: &mut SymbolMap, variable: Variable) {
        let name = variable.name.clone();
        let id = symbol_map.add_variable(variable);
        let current_scope = self.scopes.last_mut().expect("scope is empty");
        current_scope.name_to_variable.insert(name, id);
    }

    pub fn find_local(&self, name: &EcoString) -> Option<SymbolId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.find_variable(name) {
                return Some(id.into());
            }
            if let Some(record) = scope.record() {
                if let Some(field_id) = record.find_field(name) {
                    return Some(field_id.into());
                }
                if let Some(template_arg_id) = record.find_template_arg(name) {
                    return Some(template_arg_id.into());
                }
            }
        }
        None
    }

    pub fn find_variable_in_current_scope(&self, name: &EcoString) -> Option<VariableId> {
        let scope = self.scopes.last().expect("scope is empty");
        scope.find_variable(name)
    }
}

#[derive(Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub name_to_variable: HashMap<EcoString, VariableId>,
}

#[derive(Debug)]
pub enum ScopeKind {
    Root,
    Class(ClassId, Record),
    Def(DefId, Record),
    Foreach(EcoString, VariableId),
}

impl Scope {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            name_to_variable: HashMap::new(),
        }
    }

    pub fn record_id(&self) -> Option<SymbolId> {
        match self.kind {
            ScopeKind::Root => None,
            ScopeKind::Class(id, _) => Some(id.into()),
            ScopeKind::Def(id, _) => Some(id.into()),
            ScopeKind::Foreach(_, _) => None,
        }
    }

    pub fn record(&self) -> Option<&Record> {
        match &self.kind {
            ScopeKind::Root => None,
            ScopeKind::Class(_, record) => Some(record),
            ScopeKind::Def(_, record) => Some(record),
            ScopeKind::Foreach(_, _) => None,
        }
    }

    pub fn record_mut(&mut self) -> Option<&mut Record> {
        match &mut self.kind {
            ScopeKind::Root => None,
            ScopeKind::Class(_, record) => Some(record),
            ScopeKind::Def(_, record) => Some(record),
            ScopeKind::Foreach(_, _) => None,
        }
    }

    pub fn into_class(self) -> (ClassId, Record) {
        match self.kind {
            ScopeKind::Class(id, class) => (id, class),
            _ => panic!("not a class"),
        }
    }

    pub fn into_def(self) -> (DefId, Record) {
        match self.kind {
            ScopeKind::Def(id, def) => (id, def),
            _ => panic!("not a def"),
        }
    }

    pub fn add_variable(&mut self, name: EcoString, variable_id: VariableId) {
        self.name_to_variable.insert(name, variable_id);
    }

    pub fn find_variable(&self, name: &EcoString) -> Option<VariableId> {
        if let Some(var_id) = self.name_to_variable.get(name) {
            return Some(*var_id);
        }

        if let ScopeKind::Foreach(var_name, var_id) = &self.kind {
            if name == var_name {
                return Some(*var_id);
            }
        }

        None
    }
}
