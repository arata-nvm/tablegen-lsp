use ecow::EcoString;
use id_arena::Id;

use crate::file_system::FileRange;

use super::expr::Expr;

pub type VariableId = Id<Variable>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Variable {
    pub name: EcoString,
    pub value: Expr,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

impl Variable {
    pub fn new(name: EcoString, value: Expr, define_loc: FileRange) -> Self {
        Self {
            name,
            value,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}
