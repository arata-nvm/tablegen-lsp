use ecow::EcoString;
use id_arena::Id;
use indexmap::IndexMap;

use crate::file_system::FileRange;

use super::{record::RecordFieldId, template_arg::TemplateArgumentId, SymbolMap};

pub type ClassId = Id<Class>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Class {
    pub name: EcoString,
    pub name_to_template_arg: IndexMap<EcoString, TemplateArgumentId>,
    pub name_to_record_field: IndexMap<EcoString, RecordFieldId>,
    pub parent_list: Vec<ClassId>,

    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

impl Class {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            name_to_template_arg: IndexMap::new(),
            name_to_record_field: IndexMap::new(),
            parent_list: Vec::new(),
            define_loc,
            reference_locs: Vec::new(),
        }
    }

    pub fn add_template_arg(&mut self, name: EcoString, template_arg_id: TemplateArgumentId) {
        self.name_to_template_arg.insert(name, template_arg_id);
    }

    pub fn iter_template_arg(&self) -> impl Iterator<Item = TemplateArgumentId> + '_ {
        self.name_to_template_arg.values().copied()
    }

    pub fn find_template_arg(&self, name: &EcoString) -> Option<TemplateArgumentId> {
        self.name_to_template_arg.get(name).copied()
    }

    pub fn add_record_field(&mut self, name: EcoString, record_field_id: RecordFieldId) {
        self.name_to_record_field.insert(name, record_field_id);
    }

    pub fn iter_field(&self) -> impl Iterator<Item = RecordFieldId> + '_ {
        self.name_to_record_field.values().copied()
    }

    pub fn find_field(&self, symbol_map: &SymbolMap, name: &EcoString) -> Option<RecordFieldId> {
        if let Some(field_id) = self.name_to_record_field.get(name) {
            return Some(*field_id);
        }

        for parent_id in &self.parent_list {
            let parent = symbol_map.class(*parent_id);
            if let Some(field_id) = parent.find_field(symbol_map, name) {
                return Some(field_id);
            }
        }

        None
    }

    pub fn add_parent(&mut self, parent_id: ClassId) {
        self.parent_list.push(parent_id);
    }

    pub fn is_subclass_of(&self, symbol_map: &SymbolMap, other_id: ClassId) -> bool {
        if self.parent_list.contains(&other_id) {
            return true;
        }

        for parent_id in &self.parent_list {
            let parent = symbol_map.class(*parent_id);
            if parent.is_subclass_of(symbol_map, other_id) {
                return true;
            }
        }

        false
    }
}
