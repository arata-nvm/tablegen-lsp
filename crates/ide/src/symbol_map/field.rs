use ecow::EcoString;
use id_arena::Id;

use crate::file_system::FileRange;

use super::{expr::Expr, symbol::SymbolId, typ::Type};

pub type FieldId = Id<Field>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Field {
    pub name: EcoString,
    pub typ: Type,
    pub expr: Expr,
    pub parent: SymbolId,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

impl Field {
    pub fn new(
        name: EcoString,
        typ: Type,
        expr: Expr,
        parent: SymbolId,
        define_loc: FileRange,
    ) -> Self {
        Self {
            name,
            typ,
            expr,
            parent,
            define_loc,
            reference_locs: Vec::new(),
        }
    }

    pub fn modified(&self, expr: Expr, parent: SymbolId, define_loc: FileRange) -> Self {
        Field::new(
            self.name.clone(),
            self.typ.clone(),
            expr,
            parent,
            define_loc,
        )
    }
}
