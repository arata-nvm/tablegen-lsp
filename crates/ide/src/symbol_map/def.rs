use ecow::EcoString;
use id_arena::Id;
use indexmap::IndexMap;

use crate::{
    eval::{context::EvalCtx, EvalExpr},
    file_system::FileRange,
};

use super::{class::ClassId, field::Field, symbol::SymbolId, typ::Type, value::Value};

pub type DefId = Id<Def>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Def {
    pub name: EcoString,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
    pub name_to_def_field: IndexMap<EcoString, DefFieldId>,
    pub parent_class_list: Vec<ClassId>,
}

pub type DefFieldId = Id<DefField>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DefField {
    pub name: EcoString,
    pub typ: Type,
    pub value: Value,
    pub parent: SymbolId,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

impl Def {
    pub fn iter_field(&self) -> impl Iterator<Item = DefFieldId> + '_ {
        self.name_to_def_field.values().copied()
    }

    pub fn find_field(&self, name: &EcoString) -> Option<DefFieldId> {
        self.name_to_def_field.get(name).copied()
    }
}

impl DefField {
    pub fn new(
        name: EcoString,
        typ: Type,
        value: Value,
        parent: SymbolId,
        define_loc: FileRange,
    ) -> Self {
        Self {
            name,
            typ,
            value,
            parent,
            define_loc,
            reference_locs: Vec::new(),
        }
    }

    pub fn from_field(field: Field, ctx: &mut EvalCtx) -> Self {
        Self {
            name: field.name,
            typ: field.typ,
            value: field.expr.eval_expr(ctx).unwrap_or_default(),
            parent: field.parent,
            define_loc: field.define_loc,
            reference_locs: field.reference_locs,
        }
    }
}
