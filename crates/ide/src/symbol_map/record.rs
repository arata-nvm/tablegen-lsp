use std::collections::HashSet;

use ecow::EcoString;
use id_arena::Id;
use indexmap::IndexMap;

use crate::file_system::FileRange;

use super::{
    SymbolMap,
    class::{Class, ClassId},
    def::{Def, DefId},
    typ::Type,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RecordData {
    name: EcoString,
    name_to_record_field: IndexMap<EcoString, RecordFieldId>,
    parent_classes: Vec<ClassId>,
    define_loc: FileRange,
    reference_locs: Vec<FileRange>,
}

impl RecordData {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            name_to_record_field: IndexMap::new(),
            parent_classes: Vec::new(),
            define_loc,
            reference_locs: Vec::new(),
        }
    }

    pub fn clone_with(&self, name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            name_to_record_field: self.name_to_record_field.clone(),
            parent_classes: self.parent_classes.clone(),
            define_loc,
            reference_locs: self.reference_locs.clone(),
        }
    }
}

pub trait AsRecordData: Sized {
    fn record_data(&self) -> &RecordData;

    fn name(&self) -> &EcoString {
        &self.record_data().name
    }

    fn define_loc(&self) -> &FileRange {
        &self.record_data().define_loc
    }

    fn reference_locs(&self) -> &[FileRange] {
        &self.record_data().reference_locs
    }

    fn parent_classes(&self) -> &[ClassId] {
        &self.record_data().parent_classes
    }

    fn iter_direct_field(&self) -> impl Iterator<Item = RecordFieldId> + '_ {
        self.record_data().name_to_record_field.values().copied()
    }

    fn iter_field(&self, symbol_map: &SymbolMap) -> impl Iterator<Item = RecordFieldId> + '_ {
        collect_all_fields(self, symbol_map).into_iter()
    }

    fn find_field(&self, symbol_map: &SymbolMap, name: &EcoString) -> Option<RecordFieldId> {
        let RecordData {
            name_to_record_field,
            parent_classes,
            ..
        } = self.record_data();

        if let Some(field_id) = name_to_record_field.get(name) {
            return Some(*field_id);
        }

        for parent_id in parent_classes {
            let parent = symbol_map.class(*parent_id);
            if let Some(field_id) = <Class as AsRecordData>::find_field(parent, symbol_map, name) {
                return Some(field_id);
            }
        }

        None
    }

    fn is_subclass_of(&self, symbol_map: &SymbolMap, other_id: ClassId) -> bool {
        let RecordData { parent_classes, .. } = self.record_data();

        if parent_classes.contains(&other_id) {
            return true;
        }

        parent_classes
            .iter()
            .map(|parent_id| symbol_map.class(*parent_id))
            .any(|parent| parent.is_subclass_of(symbol_map, other_id))
    }
}

pub trait AsRecordDataMut: AsRecordData {
    fn record_data_mut(&mut self) -> &mut RecordData;

    fn add_reference_loc(&mut self, reference_loc: FileRange) {
        self.record_data_mut().reference_locs.push(reference_loc);
    }

    fn add_record_field(&mut self, name: EcoString, record_field_id: RecordFieldId) {
        self.record_data_mut()
            .name_to_record_field
            .insert(name, record_field_id);
    }

    /// callers must ensure that the parent is not already inherited
    fn add_parent(&mut self, parent_id: ClassId) {
        self.record_data_mut().parent_classes.push(parent_id);
    }
}

fn collect_all_fields<T: AsRecordData>(
    record: &T,
    symbol_map: &SymbolMap,
) -> HashSet<RecordFieldId> {
    let mut found_fields = HashSet::new();
    // Add direct fields
    for field_id in record.iter_direct_field() {
        found_fields.insert(field_id);
    }
    // Add parent fields
    for parent_id in record.parent_classes() {
        let parent = symbol_map.class(*parent_id);
        found_fields.extend(collect_all_fields(parent, symbol_map));
    }
    found_fields
}

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

impl<'a> AsRecordData for Record<'a> {
    fn record_data(&self) -> &RecordData {
        match self {
            Self::Class(class) => class.record_data(),
            Self::Def(def) => def.record_data(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum RecordMut<'a> {
    Class(&'a mut Class),
    Def(&'a mut Def),
}

impl<'a> AsRecordData for RecordMut<'a> {
    fn record_data(&self) -> &RecordData {
        match self {
            Self::Class(class) => class.record_data(),
            Self::Def(def) => def.record_data(),
        }
    }
}

impl<'a> AsRecordDataMut for RecordMut<'a> {
    fn record_data_mut(&mut self) -> &mut RecordData {
        match self {
            Self::Class(class) => class.record_data_mut(),
            Self::Def(def) => def.record_data_mut(),
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
