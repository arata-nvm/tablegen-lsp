use std::collections::HashMap;

use ecow::EcoString;

use crate::symbol_map::{
    defm::DefmId,
    defset::DefsetId,
    multiclass::MulticlassId,
    record::RecordId,
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

    pub fn current_record_id(&self) -> Option<RecordId> {
        self.scopes.iter().rev().find_map(|scope| scope.record_id())
    }

    pub fn current_defset_id(&self) -> Option<DefsetId> {
        self.scopes.iter().rev().find_map(|scope| scope.defset_id())
    }

    pub fn current_multiclass_id(&self) -> Option<MulticlassId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.multiclass_id())
    }

    pub fn current_defm_id(&self) -> Option<DefmId> {
        self.scopes.iter().rev().find_map(|scope| scope.defm_id())
    }

    pub fn add_variable(&mut self, symbol_map: &mut SymbolMap, variable: Variable) {
        let name = variable.name.clone();
        let id = symbol_map.add_variable(variable);
        let current_scope = self.scopes.last_mut().expect("scope is empty");
        current_scope.name_to_variable.insert(name, id);
    }

    pub fn find_local(&self, symbol_map: &SymbolMap, name: &EcoString) -> Option<SymbolId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.find_variable(name) {
                return Some(id.into());
            }
            if let Some(record_id) = scope.record_id() {
                let record = symbol_map.record(record_id);
                if let Some(field_id) = record.find_field(symbol_map, name) {
                    return Some(field_id.into());
                }
                if let Some(template_arg_id) = record.find_template_arg(name) {
                    return Some(template_arg_id.into());
                }
            }
            if let Some(multiclass_id) = scope.multiclass_id() {
                let multiclass = symbol_map.multiclass(multiclass_id);
                if let Some(template_arg_id) = multiclass.find_template_arg(name) {
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
    Record(RecordId),
    Foreach(EcoString, VariableId),
    Defset(DefsetId),
    Multiclass(MulticlassId),
    Defm(DefmId),
    XFilter,
    XFoldl,
    XForeach,
}

impl Scope {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            name_to_variable: HashMap::new(),
        }
    }

    pub fn record_id(&self) -> Option<RecordId> {
        match self.kind {
            ScopeKind::Record(id) => Some(id),
            _ => None,
        }
    }

    pub fn defset_id(&self) -> Option<DefsetId> {
        match self.kind {
            ScopeKind::Defset(id) => Some(id),
            _ => None,
        }
    }

    pub fn multiclass_id(&self) -> Option<MulticlassId> {
        match self.kind {
            ScopeKind::Multiclass(id) => Some(id),
            _ => None,
        }
    }

    pub fn defm_id(&self) -> Option<DefmId> {
        match self.kind {
            ScopeKind::Defm(id) => Some(id),
            _ => None,
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
