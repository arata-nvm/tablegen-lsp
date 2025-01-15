use ecow::EcoString;
use id_arena::Id;
use indexmap::IndexMap;

use crate::file_system::FileRange;

use super::{
    class::ClassId,
    field::{Field, FieldId},
    SymbolMap,
};

pub type DefId = Id<Def>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Def {
    pub name: EcoString,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
    pub name_to_def_field: IndexMap<EcoString, FieldId>,
    pub parent_class_list: Vec<ClassId>,
}

impl Def {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            define_loc,
            reference_locs: Vec::new(),
            name_to_def_field: IndexMap::new(),
            parent_class_list: Vec::new(),
        }
    }

    pub fn add_field(&mut self, symbol_map: &mut SymbolMap, new_field: Field) -> FieldId {
        let name = new_field.name.clone();
        let id = symbol_map.add_field(new_field);
        self.name_to_def_field.insert(name, id);
        id
    }

    pub fn iter_field(&self) -> impl Iterator<Item = FieldId> + '_ {
        self.name_to_def_field.values().copied()
    }

    pub fn find_field(&self, name: &EcoString) -> Option<FieldId> {
        self.name_to_def_field.get(name).copied()
    }
}
