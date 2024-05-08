use std::collections::HashMap;

use ecow::EcoString;
use id_arena::{Arena, Id};

use crate::file_system::{FileId, FileLocation, FileRange};

#[derive(Debug, Eq, PartialEq)]
pub struct Class {
    pub name: EcoString,
    pub define_range: FileRange,
    pub define_loc: FileLocation,
}

pub type ClassId = Id<Class>;

impl Class {
    pub fn new(name: EcoString, define_range: FileRange, define_loc: FileLocation) -> Self {
        Self {
            name,
            define_range,
            define_loc,
        }
    }
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct SymbolMap {
    class_list: Arena<Class>,
    file_to_class_list: HashMap<FileId, Vec<ClassId>>,
}

// immutable api
impl SymbolMap {
    pub fn new() -> Self {
        Self::default()
    }
}

// mutable api
impl SymbolMap {
    pub fn add_class(&mut self, class: Class, file_id: FileId) -> ClassId {
        let id = self.class_list.alloc(class);
        self.file_to_class_list
            .entry(file_id)
            .or_insert_with(Vec::new)
            .push(id);
        id
    }

    pub fn class(&self, class_id: ClassId) -> Option<&Class> {
        self.class_list.get(class_id)
    }

    pub fn iter_class(&self) -> impl Iterator<Item = (ClassId, &Class)> {
        self.class_list.iter()
    }

    pub fn iter_class_in(&self, file_id: FileId) -> Option<impl Iterator<Item = ClassId>> {
        self.file_to_class_list
            .get(&file_id)
            .cloned()
            .map(|class_list| class_list.into_iter())
    }
}
