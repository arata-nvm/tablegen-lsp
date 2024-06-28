use ecow::EcoString;

use crate::file_system::FileRange;

use super::{
    class::{Class, ClassId},
    def::{Def, DefField, DefFieldId, DefId},
    field::{Field, FieldId},
    template_arg::{TemplateArgument, TemplateArgumentId},
    variable::{Variable, VariableId},
};

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash, PartialOrd, Ord)]
pub enum SymbolId {
    ClassId(ClassId),
    TemplateArgumentId(TemplateArgumentId),
    FieldId(FieldId),
    DefId(DefId),
    VariableId(VariableId),
    DefFieldId(DefFieldId),
}

impl From<ClassId> for SymbolId {
    fn from(id: ClassId) -> Self {
        SymbolId::ClassId(id)
    }
}

impl From<TemplateArgumentId> for SymbolId {
    fn from(id: TemplateArgumentId) -> Self {
        SymbolId::TemplateArgumentId(id)
    }
}

impl From<FieldId> for SymbolId {
    fn from(id: FieldId) -> Self {
        SymbolId::FieldId(id)
    }
}

impl From<DefId> for SymbolId {
    fn from(id: DefId) -> Self {
        SymbolId::DefId(id)
    }
}

impl From<VariableId> for SymbolId {
    fn from(id: VariableId) -> Self {
        SymbolId::VariableId(id)
    }
}

impl From<DefFieldId> for SymbolId {
    fn from(id: DefFieldId) -> Self {
        SymbolId::DefFieldId(id)
    }
}

impl SymbolId {
    pub fn as_class_id(&self) -> Option<ClassId> {
        match self {
            SymbolId::ClassId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_template_argument_id(&self) -> Option<TemplateArgumentId> {
        match self {
            SymbolId::TemplateArgumentId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_field_id(&self) -> Option<FieldId> {
        match self {
            SymbolId::FieldId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_def_id(&self) -> Option<DefId> {
        match self {
            SymbolId::DefId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_variable_id(&self) -> Option<VariableId> {
        match self {
            SymbolId::VariableId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_def_field_id(&self) -> Option<DefFieldId> {
        match self {
            SymbolId::DefFieldId(id) => Some(*id),
            _ => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Symbol<'a> {
    Class(&'a Class),
    TemplateArgument(&'a TemplateArgument),
    Field(&'a Field),
    Def(&'a Def),
    Variable(&'a Variable),
    DefField(&'a DefField),
}

impl<'a> Symbol<'a> {
    pub fn name(&self) -> &EcoString {
        match self {
            Self::Class(class) => &class.name,
            Self::TemplateArgument(template_arg) => &template_arg.name,
            Self::Field(field) => &field.name,
            Self::Def(def) => &def.name,
            Self::Variable(variable) => &variable.name,
            Self::DefField(def_field) => &def_field.name,
        }
    }

    pub fn define_loc(&self) -> &FileRange {
        match self {
            Self::Class(class) => &class.define_loc,
            Self::TemplateArgument(template_arg) => &template_arg.define_loc,
            Self::Field(field) => &field.define_loc,
            Self::Def(def) => &def.define_loc,
            Self::Variable(variable) => &variable.define_loc,
            Self::DefField(def_field) => &def_field.define_loc,
        }
    }

    pub fn reference_locs(&self) -> &[FileRange] {
        match self {
            Self::Class(class) => &class.reference_locs,
            Self::TemplateArgument(template_arg) => &template_arg.reference_locs,
            Self::Field(field) => &field.reference_locs,
            Self::Def(def) => &def.reference_locs,
            Self::Variable(variable) => &variable.reference_locs,
            Self::DefField(def_field) => &def_field.reference_locs,
        }
    }

    pub fn as_class(&self) -> Option<&Class> {
        match self {
            Self::Class(class) => Some(class),
            _ => None,
        }
    }

    pub fn as_template_argument(&self) -> Option<&TemplateArgument> {
        match self {
            Self::TemplateArgument(template_arg) => Some(template_arg),
            _ => None,
        }
    }

    pub fn as_field(&self) -> Option<&Field> {
        match self {
            Self::Field(field) => Some(field),
            _ => None,
        }
    }

    pub fn as_def(&self) -> Option<&Def> {
        match self {
            Self::Def(def) => Some(def),
            _ => None,
        }
    }

    pub fn as_variable(&self) -> Option<&Variable> {
        match self {
            Self::Variable(variable) => Some(variable),
            _ => None,
        }
    }

    pub fn as_def_field(&self) -> Option<&DefField> {
        match self {
            Self::DefField(def_field) => Some(def_field),
            _ => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum SymbolMut<'a> {
    Class(&'a mut Class),
    TemplateArgument(&'a mut TemplateArgument),
    Field(&'a mut Field),
    Def(&'a mut Def),
    Variable(&'a mut Variable),
    DefField(&'a mut DefField),
}

impl<'a> SymbolMut<'a> {
    pub fn add_reference(&mut self, reference_loc: FileRange) {
        match self {
            Self::Class(class) => class.reference_locs.push(reference_loc),
            Self::TemplateArgument(template_arg) => template_arg.reference_locs.push(reference_loc),
            Self::Field(field) => field.reference_locs.push(reference_loc),
            Self::Def(def) => def.reference_locs.push(reference_loc),
            Self::Variable(variable) => variable.reference_locs.push(reference_loc),
            Self::DefField(def_field) => def_field.reference_locs.push(reference_loc),
        }
    }
}
