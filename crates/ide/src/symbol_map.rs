use ecow::EcoString;
use id_arena::{Arena, Id};

#[derive(Debug, Eq, PartialEq)]
pub struct Class {
    pub name: EcoString,
}

pub type ClassId = Id<Class>;

impl Class {
    pub fn new(name: EcoString) -> Self {
        Self { name }
    }
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct SymbolMap {
    classes: Arena<Class>,
}

// immutable api
impl SymbolMap {
    pub fn new() -> Self {
        Self::default()
    }
}

// mutable api
impl SymbolMap {
    pub fn add_class(&mut self, class: Class) -> ClassId {
        self.classes.alloc(class)
    }

    pub fn class(&self, class_id: ClassId) -> Option<&Class> {
        self.classes.get(class_id)
    }

    pub fn iter_class(&self) -> impl Iterator<Item = (ClassId, &Class)> {
        self.classes.iter()
    }
}
