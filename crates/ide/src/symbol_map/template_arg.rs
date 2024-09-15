use ecow::EcoString;
use id_arena::Id;

use crate::file_system::FileRange;

pub type TemplateArgumentId = Id<TemplateArgument>;

#[derive(Debug, Eq, PartialEq)]
pub struct TemplateArgument {
    pub name: EcoString,
    // pub typ: Type,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

impl TemplateArgument {
    pub fn new(
        name: EcoString,
        // typ: Type,
        define_loc: FileRange,
    ) -> Self {
        Self {
            name,
            // typ,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}
