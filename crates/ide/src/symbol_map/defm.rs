use ecow::EcoString;
use id_arena::Id;

use crate::file_system::FileRange;

use super::multiclass::MulticlassId;

pub type DefmId = Id<Defm>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Defm {
    pub name: EcoString,
    pub parent_list: Vec<MulticlassId>,

    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

impl Defm {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            parent_list: Vec::new(),
            define_loc,
            reference_locs: Vec::new(),
        }
    }

    pub fn add_parent(&mut self, parent_id: MulticlassId) {
        self.parent_list.push(parent_id);
    }
}
