use ecow::EcoString;
use id_arena::Id;

use crate::file_system::FileRange;

use super::typ::Type;

pub type VariableId = Id<Variable>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Variable {
    pub name: EcoString,
    pub typ: Type,
    pub kind: VariableKind,

    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VariableKind {
    Defvar,
    Foreach,
    XFilter,
    XFoldl,
    XForeach,
    XSort,
}

impl Variable {
    pub fn new(name: EcoString, typ: Type, kind: VariableKind, define_loc: FileRange) -> Self {
        Self {
            name,
            typ,
            kind,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}
