use ecow::EcoString;
use id_arena::Id;

use crate::file_system::FileRange;

use super::typ::Type;

pub type TemplateArgumentId = Id<TemplateArgument>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TemplateArgument {
    pub name: EcoString,
    pub typ: Type,
    pub has_default_value: bool,

    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

impl TemplateArgument {
    pub fn new(name: EcoString, typ: Type, has_default_value: bool, define_loc: FileRange) -> Self {
        Self {
            name,
            typ,
            has_default_value,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}
