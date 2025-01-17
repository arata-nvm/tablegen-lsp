use ecow::EcoString;
use id_arena::Id;

use crate::file_system::FileRange;

use super::{record::RecordId, typ::Type};

pub type DefsetId = Id<Defset>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Defset {
    pub name: EcoString,
    pub typ: Type,
    pub def_list: Vec<RecordId>,

    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

impl Defset {
    pub fn new(name: EcoString, typ: Type, define_loc: FileRange) -> Self {
        Self {
            name,
            typ,
            def_list: Vec::new(),
            define_loc,
            reference_locs: Vec::new(),
        }
    }

    pub fn add_def(&mut self, record_id: RecordId) {
        self.def_list.push(record_id);
    }
}
