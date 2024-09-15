use ecow::EcoString;
use id_arena::Id;

use crate::file_system::FileRange;

pub type FieldId = Id<Field>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Field {
    pub name: EcoString,
    // pub typ: Type,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

impl Field {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            // typ,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}
