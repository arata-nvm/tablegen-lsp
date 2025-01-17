use ecow::EcoString;
use id_arena::Id;
use indexmap::IndexMap;

use crate::file_system::FileRange;

use super::{record_field::RecordFieldId, template_arg::TemplateArgumentId, SymbolMap};

pub type RecordId = Id<Record>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record {
    pub name: EcoString,
    pub kind: RecordKind,
    pub name_to_template_arg: IndexMap<EcoString, TemplateArgumentId>,
    pub name_to_record_field: IndexMap<EcoString, RecordFieldId>,
    pub parent_list: Vec<RecordId>,

    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RecordKind {
    Class,
    Def,
}

impl Record {
    pub fn new(name: EcoString, kind: RecordKind, define_loc: FileRange) -> Self {
        Self {
            name,
            kind,
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
            let parent = symbol_map.record(*parent_id);
            if let Some(field_id) = parent.find_field(symbol_map, name) {
                return Some(field_id);
            }
        }

        None
    }

    pub fn add_parent(&mut self, parent_id: RecordId) {
        self.parent_list.push(parent_id);
    }
}
