use ecow::EcoString;
use id_arena::Id;

use crate::file_system::FileRange;

use super::{expr::Expr, typ::Type};

pub type TemplateArgumentId = Id<TemplateArgument>;

#[derive(Debug, Eq, PartialEq)]
pub struct TemplateArgument {
    pub name: EcoString,
    pub typ: Type,
    pub default_value: Option<Expr>,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

impl TemplateArgument {
    pub fn new(
        name: EcoString,
        typ: Type,
        default_value: Option<Expr>,
        define_loc: FileRange,
    ) -> Self {
        Self {
            name,
            typ,
            default_value,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}
