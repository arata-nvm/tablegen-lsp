use ecow::EcoString;
use id_arena::Id;
use indexmap::IndexMap;

use crate::file_system::FileRange;

use super::{field::FieldId, template_arg::TemplateArgumentId};

pub type ClassId = Id<Class>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Class {
    pub name: EcoString,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
    pub name_to_template_arg: IndexMap<EcoString, TemplateArgumentId>,
    pub name_to_field: IndexMap<EcoString, FieldId>,
    pub parent_class_list: Vec<ClassId>,
}

impl Class {
    pub fn iter_template_arg(&self) -> impl Iterator<Item = TemplateArgumentId> + '_ {
        self.name_to_template_arg.values().copied()
    }

    pub fn find_template_arg(&self, name: &EcoString) -> Option<TemplateArgumentId> {
        self.name_to_template_arg.get(name).copied()
    }

    pub fn iter_field(&self) -> impl Iterator<Item = FieldId> + '_ {
        self.name_to_field.values().copied()
    }

    pub fn find_field(&self, name: &EcoString) -> Option<FieldId> {
        self.name_to_field.get(name).copied()
    }
}
