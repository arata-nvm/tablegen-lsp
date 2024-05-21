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
    pub template_arg_list: Vec<TemplateArgumentId>,
    pub parent_class_list: Vec<ClassId>,
}

pub type ClassId = Id<Class>;

impl Class {
    pub fn new(
        name: EcoString,
        define_loc: FileRange,
        template_arg_list: Vec<TemplateArgumentId>,
        parent_class_list: Vec<ClassId>,
    ) -> Self {
        Self {
            name,
            define_loc,
            reference_locs: Vec::new(),
            template_arg_list,
            parent_class_list,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct TemplateArgument {
    pub name: EcoString,
    pub define_loc: FileRange,
}

pub type TemplateArgumentId = Id<TemplateArgument>;

impl TemplateArgument {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self { name, define_loc }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Symbol<'a> {
    Class(&'a Class),
    TemplateArgument(&'a TemplateArgument),
}

impl<'a> Symbol<'a> {
    pub fn name(&self) -> &EcoString {
        match self {
            Symbol::Class(class) => &class.name,
            Symbol::TemplateArgument(template_arg) => &template_arg.name,
        }
    }

    pub fn define_loc(&self) -> &FileRange {
        match self {
            Symbol::Class(class) => &class.define_loc,
            Symbol::TemplateArgument(template_arg) => &template_arg.define_loc,
        }
    }

    pub fn as_class(&self) -> Option<&Class> {
        match self {
            Symbol::Class(class) => Some(class),
            _ => None,
        }
    }

    pub fn as_template_argument(&self) -> Option<&TemplateArgument> {
        match self {
            Symbol::TemplateArgument(template_arg) => Some(template_arg),
            _ => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash, PartialOrd, Ord)]
pub enum SymbolId {
    ClassId(ClassId),
    TemplateArgumentId(TemplateArgumentId),
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct SymbolMap {
    class_list: Arena<Class>,
    template_arg_list: Arena<TemplateArgument>,
    file_to_class_list: HashMap<FileId, Vec<ClassId>>,
    pos_to_symbol_map: HashMap<FileId, IntervalMap<TextSize, SymbolId>>,
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
        let define_loc = class.define_loc;
        let id = self.class_list.alloc(class);
        self.file_to_class_list.entry(file_id).or_default().push(id);
        self.pos_to_symbol_map
            .entry(file_id)
            .or_insert_with(IntervalMap::new)
            .insert(define_loc.range.into(), SymbolId::ClassId(id));
        id
    }

    pub fn add_class_reference(&mut self, class_id: ClassId, reference_loc: FileRange) {
        let class = self.class_mut(class_id);
        class.reference_locs.push(reference_loc);
        self.pos_to_symbol_map
            .entry(reference_loc.file)
            .or_insert_with(IntervalMap::new)
            .insert(reference_loc.range.into(), SymbolId::ClassId(class_id));
    }

    pub fn add_template_argument(
        &mut self,
        template_arg: TemplateArgument,
        file_id: FileId,
    ) -> TemplateArgumentId {
        let define_loc = template_arg.define_loc;
        let id = self.template_arg_list.alloc(template_arg);
        self.pos_to_symbol_map
            .entry(file_id)
            .or_insert_with(IntervalMap::new)
            .insert(define_loc.range.into(), SymbolId::TemplateArgumentId(id));
        id
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

    pub fn find_symbol_at(&self, pos: FilePosition) -> Option<Symbol> {
        let id = self
            .pos_to_symbol_map
            .get(&pos.file)
            .and_then(|map| map.values_overlap(pos.position).next().cloned())?;

        match id {
            SymbolId::ClassId(class_id) => Some(Symbol::Class(self.class(class_id))),
            SymbolId::TemplateArgumentId(template_arg_id) => {
                Some(Symbol::TemplateArgument(self.template_arg(template_arg_id)))
            }
        }
    }

    pub fn template_arg(&self, template_arg_id: TemplateArgumentId) -> &TemplateArgument {
        self.template_arg_list
            .get(template_arg_id)
            .expect("invalid template argument id")
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
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> Eq for IntervalMap<T, V> {}
