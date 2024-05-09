use std::fmt::Debug;
use std::ops::DerefMut;
use std::{collections::HashMap, ops::Deref};

use ecow::EcoString;
use id_arena::{Arena, Id};
use syntax::parser::TextSize;

use crate::file_system::{FileId, FilePosition, FileRange};

#[derive(Debug, Eq, PartialEq)]
pub struct Class {
    pub name: EcoString,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

pub type ClassId = Id<Class>;

impl Class {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct SymbolMap {
    class_list: Arena<Class>,
    file_to_class_list: HashMap<FileId, Vec<ClassId>>,
    pos_to_class_map: HashMap<FileId, IntervalMap<TextSize, ClassId>>,
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
        let define_loc = class.define_loc.clone();
        let id = self.class_list.alloc(class);
        self.file_to_class_list
            .entry(file_id)
            .or_insert_with(Vec::new)
            .push(id);
        self.pos_to_class_map
            .entry(file_id)
            .or_insert_with(IntervalMap::new)
            .insert(define_loc.range.into(), id);
        id
    }

    pub fn add_class_reference(&mut self, class_id: ClassId, reference_loc: FileRange) {
        let class = self.class_mut(class_id);
        class.reference_locs.push(reference_loc);
        self.pos_to_class_map
            .entry(reference_loc.file)
            .or_insert_with(IntervalMap::new)
            .insert(reference_loc.range.into(), class_id);
    }

    pub fn class(&self, class_id: ClassId) -> &Class {
        self.class_list.get(class_id).expect("invalid class id")
    }

    pub fn class_mut(&mut self, class_id: ClassId) -> &mut Class {
        self.class_list.get_mut(class_id).expect("invalid class id")
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

    pub fn find_class_at(&self, pos: FilePosition) -> Option<ClassId> {
        self.pos_to_class_map
            .get(&pos.file)
            .and_then(|map| map.values_overlap(pos.position.into()).next().cloned())
    }
}

// FIXME
// because iset::IntervalMap does not implement PartialEq and Eq, we have to implement them
#[derive(Debug, Default)]
struct IntervalMap<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq>(
    iset::IntervalMap<T, V>,
);

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> IntervalMap<T, V> {
    fn new() -> Self {
        Self(iset::IntervalMap::new())
    }
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> Deref for IntervalMap<T, V> {
    type Target = iset::IntervalMap<T, V>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> DerefMut for IntervalMap<T, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> PartialEq for IntervalMap<T, V> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }

        for ((range1, value1), (range2, value2)) in self.0.iter(..).zip(other.0.iter(..)) {
            if range1 != range2 || value1 != value2 {
                return false;
            }
        }
        true
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> Eq for IntervalMap<T, V> {}
