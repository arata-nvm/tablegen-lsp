use ecow::EcoString;
use id_arena::Id;

use crate::file_system::FileRange;

use super::{
    class::{Class, ClassId},
    def::{Def, DefId},
    typ::Type,
    SymbolMap,
};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RecordId {
    Class(ClassId),
    Def(DefId),
}

impl From<ClassId> for RecordId {
    fn from(class_id: ClassId) -> Self {
        Self::Class(class_id)
    }
}

impl From<DefId> for RecordId {
    fn from(def_id: DefId) -> Self {
        Self::Def(def_id)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Record<'a> {
    Class(&'a Class),
    Def(&'a Def),
}

impl<'a> Record<'a> {
    pub fn name(&self) -> &EcoString {
        match self {
            Self::Class(class) => &class.name,
            Self::Def(def) => &def.name,
        }
    }

    pub fn find_field(&self, symbol_map: &SymbolMap, name: &EcoString) -> Option<RecordFieldId> {
        match self {
            Self::Class(class) => class.find_field(symbol_map, name),
            Self::Def(def) => def.find_field(symbol_map, name),
        }
    }

    pub fn is_subclass_of(&self, symbol_map: &SymbolMap, other_id: ClassId) -> bool {
        match self {
            Self::Class(class) => class.is_subclass_of(symbol_map, other_id),
            Self::Def(def) => def.is_subclass_of(symbol_map, other_id),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum RecordMut<'a> {
    Class(&'a mut Class),
    Def(&'a mut Def),
}

impl<'a> RecordMut<'a> {
    pub fn add_record_field(&mut self, name: EcoString, record_field_id: RecordFieldId) {
        match self {
            Self::Class(class) => class.add_record_field(name, record_field_id),
            Self::Def(def) => def.add_record_field(name, record_field_id),
        }
    }

    pub(super) fn add_parent(&mut self, parent_id: ClassId) {
        match self {
            Self::Class(class) => class.add_parent(parent_id),
            Self::Def(def) => def.add_parent(parent_id),
        }
    }
}

pub type RecordFieldId = Id<RecordField>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RecordField {
    pub name: EcoString,
    pub typ: Type,
    pub parent: RecordId,

    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

impl RecordField {
    pub fn new(name: EcoString, typ: Type, parent: RecordId, define_loc: FileRange) -> Self {
        Self {
            name,
            typ,
            parent,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}
