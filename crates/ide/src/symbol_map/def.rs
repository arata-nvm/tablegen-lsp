use ecow::EcoString;
use id_arena::Id;
use indexmap::IndexMap;

use crate::file_system::FileRange;

use super::{class::ClassId, field::FieldId};

pub type DefId = Id<Def>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Def {
    pub name: EcoString,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
    pub name_to_field: IndexMap<EcoString, FieldId>,
    pub parent_class_list: Vec<ClassId>,
}

impl Def {
    pub fn iter_field(&self) -> impl Iterator<Item = FieldId> + '_ {
        self.name_to_field.values().copied()
    }

    pub fn find_field(&self, name: &EcoString) -> Option<FieldId> {
        self.name_to_field.get(name).copied()
    }
}
