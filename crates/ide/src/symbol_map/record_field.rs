use ecow::EcoString;
use id_arena::Id;

use crate::file_system::FileRange;

use super::{record::RecordId, typ::Type};

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
