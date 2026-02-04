use std::collections::HashMap;

use ecow::EcoString;

use crate::symbol_map::{
    SymbolMap,
    class::ClassId,
    def::DefId,
    defm::DefmId,
    defset::DefsetId,
    multiclass::MulticlassId,
    record::{AsRecordData, RecordId},
    symbol::SymbolId,
    variable::{Variable, VariableId},
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

#[derive(Debug, thiserror::Error)]
pub enum ScopeError {
    #[error("variable '{0}' is already defined in the current scope")]
    VariableAlreadyDefined(EcoString),
}

impl Scopes {
    pub fn push(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope::new(kind));
    }

    pub fn pop(&mut self) -> Scope {
        self.scopes.pop().expect("scope is empty")
    }

    pub fn is_global_scope(&self) -> bool {
        self.scopes.len() == 1
    }

    pub fn current_class_id(&self) -> Option<ClassId> {
        self.scopes.iter().rev().find_map(|scope| scope.class_id())
    }

    pub fn current_def_id(&self) -> Option<DefId> {
        self.scopes.iter().rev().find_map(|scope| scope.def_id())
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

    pub fn add_variable(
        &mut self,
        symbol_map: &mut SymbolMap,
        variable: Variable,
    ) -> Result<(), ScopeError> {
        let found_in_global =
            self.is_global_scope() && symbol_map.find_def(&variable.name).is_some();
        let found_in_current_scope = self
            .find_variable_in_current_scope(&variable.name)
            .is_some();
        if found_in_global || found_in_current_scope {
            return Err(ScopeError::VariableAlreadyDefined(variable.name.clone()));
        }

        let name = variable.name.clone();
        let id = symbol_map.add_variable(variable);
        let current_scope = self.scopes.last_mut().expect("scope is empty");
        current_scope.name_to_variable.insert(name, id);
        Ok(())
    }

    pub fn find_local(&self, symbol_map: &SymbolMap, name: &EcoString) -> Option<SymbolId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.find_variable(name) {
                return Some(id.into());
            }
            if let Some(class_id) = scope.class_id() {
                let class = symbol_map.class(class_id);
                if let Some(field_id) = class.find_field(symbol_map, name) {
                    return Some(field_id.into());
                }
                if let Some(template_arg_id) = class.find_template_arg(name) {
                    return Some(template_arg_id.into());
                }
            }
            if let Some(def_id) = scope.def_id() {
                let def = symbol_map.def(def_id);
                if let Some(field_id) = def.find_field(symbol_map, name) {
                    return Some(field_id.into());
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
    Block,
    Class(ClassId),
    Def(DefId),
    Foreach(EcoString, VariableId),
    Defset(DefsetId),
    Multiclass(MulticlassId),
    Defm(DefmId),
    XFilter(EcoString, VariableId),
    XFoldl(EcoString, VariableId, EcoString, VariableId),
    XForeach(EcoString, VariableId),
}

impl Scope {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            name_to_variable: HashMap::new(),
        }
    }

    pub fn class_id(&self) -> Option<ClassId> {
        match self.kind {
            ScopeKind::Class(id) => Some(id),
            _ => None,
        }
    }

    pub fn def_id(&self) -> Option<DefId> {
        match self.kind {
            ScopeKind::Def(id) => Some(id),
            _ => None,
        }
    }

    pub fn record_id(&self) -> Option<RecordId> {
        match self.kind {
            ScopeKind::Class(id) => Some(id.into()),
            ScopeKind::Def(id) => Some(id.into()),
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

        match self.kind {
            ScopeKind::Foreach(ref var_name, var_id)
            | ScopeKind::XFilter(ref var_name, var_id)
            | ScopeKind::XForeach(ref var_name, var_id)
                if name == var_name =>
            {
                Some(var_id)
            }
            ScopeKind::XFoldl(ref var_name1, var_id1, ref var_name2, var_id2) => {
                if name == var_name1 {
                    Some(var_id1)
                } else if name == var_name2 {
                    Some(var_id2)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
