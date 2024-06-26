use std::collections::HashMap;

use ecow::EcoString;
use id_arena::Id;
use indexmap::IndexMap;

use crate::file_system::FileRange;

use super::{class::ClassId, field::FieldId, value::Value};

pub type DefId = Id<Def>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Def {
    pub name: EcoString,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
    pub name_to_field: IndexMap<EcoString, FieldId>,
    pub field_to_value: HashMap<FieldId, Value>,
    pub parent_class_list: Vec<ClassId>,
}

impl Def {
    pub fn iter_field(&self) -> impl Iterator<Item = FieldId> + '_ {
        self.name_to_field.values().copied()
    }

    pub fn find_field(&self, name: &EcoString) -> Option<FieldId> {
        self.name_to_field.get(name).copied()
    }

    pub fn field_value(&self, field_id: &FieldId) -> &Value {
        self.field_to_value.get(field_id).expect("invalid field id")
    }
}
