use ecow::EcoString;
use id_arena::Id;
use indexmap::IndexMap;

use crate::file_system::FileRange;

use super::{
    field::{Field, FieldId},
    template_arg::{TemplateArgument, TemplateArgumentId},
    SymbolMap,
};

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
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            define_loc,
            reference_locs: Vec::new(),
            name_to_template_arg: IndexMap::new(),
            name_to_field: IndexMap::new(),
            parent_class_list: Vec::new(),
        }
    }

    pub fn add_template_arg(
        &mut self,
        symbol_map: &mut SymbolMap,
        template_arg: TemplateArgument,
    ) -> TemplateArgumentId {
        let name = template_arg.name.clone();
        let id = symbol_map.add_template_argument(template_arg);
        self.name_to_template_arg.insert(name, id);
        id
    }

    pub fn iter_template_arg(&self) -> impl Iterator<Item = TemplateArgumentId> + '_ {
        self.name_to_template_arg.values().copied()
    }

    pub fn find_template_arg(&self, name: &EcoString) -> Option<TemplateArgumentId> {
        self.name_to_template_arg.get(name).copied()
    }

    pub fn add_field(&mut self, symbol_map: &mut SymbolMap, new_field: Field) -> FieldId {
        let name = new_field.name.clone();
        let id = symbol_map.add_field(new_field);
        self.name_to_field.insert(name, id);
        id
    }

    pub fn iter_field(&self) -> impl Iterator<Item = FieldId> + '_ {
        self.name_to_field.values().copied()
    }

    pub fn find_field(&self, name: &EcoString) -> Option<FieldId> {
        self.name_to_field.get(name).copied()
    }
}
