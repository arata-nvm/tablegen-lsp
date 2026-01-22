use ecow::EcoString;

use crate::file_system::FileRange;

use super::{
    class::{Class, ClassId},
    def::{Def, DefId},
    defm::{Defm, DefmId},
    defset::{Defset, DefsetId},
    multiclass::{Multiclass, MulticlassId},
    record::{AsRecordData, RecordField, RecordFieldId},
    template_arg::{TemplateArgument, TemplateArgumentId},
    variable::{Variable, VariableId},
};

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash, PartialOrd, Ord)]
pub enum SymbolId {
    ClassId(ClassId),
    DefId(DefId),
    TemplateArgumentId(TemplateArgumentId),
    RecordFieldId(RecordFieldId),
    VariableId(VariableId),
    DefsetId(DefsetId),
    MulticlassId(MulticlassId),
    DefmId(DefmId),
}

impl From<ClassId> for SymbolId {
    fn from(id: ClassId) -> Self {
        SymbolId::ClassId(id)
    }
}

impl From<DefId> for SymbolId {
    fn from(id: DefId) -> Self {
        SymbolId::DefId(id)
    }
}

impl From<TemplateArgumentId> for SymbolId {
    fn from(id: TemplateArgumentId) -> Self {
        SymbolId::TemplateArgumentId(id)
    }
}

impl From<RecordFieldId> for SymbolId {
    fn from(id: RecordFieldId) -> Self {
        SymbolId::RecordFieldId(id)
    }
}

impl From<VariableId> for SymbolId {
    fn from(id: VariableId) -> Self {
        SymbolId::VariableId(id)
    }
}

impl From<DefsetId> for SymbolId {
    fn from(id: DefsetId) -> Self {
        SymbolId::DefsetId(id)
    }
}

impl From<MulticlassId> for SymbolId {
    fn from(id: MulticlassId) -> Self {
        SymbolId::MulticlassId(id)
    }
}

impl From<DefmId> for SymbolId {
    fn from(id: DefmId) -> Self {
        SymbolId::DefmId(id)
    }
}

impl SymbolId {
    pub fn as_class_id(&self) -> Option<ClassId> {
        match self {
            SymbolId::ClassId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_def_id(&self) -> Option<DefId> {
        match self {
            SymbolId::DefId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_template_argument_id(&self) -> Option<TemplateArgumentId> {
        match self {
            SymbolId::TemplateArgumentId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_record_field_id(&self) -> Option<RecordFieldId> {
        match self {
            SymbolId::RecordFieldId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_variable_id(&self) -> Option<VariableId> {
        match self {
            SymbolId::VariableId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_defset_id(&self) -> Option<DefsetId> {
        match self {
            SymbolId::DefsetId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_multiclass_id(&self) -> Option<MulticlassId> {
        match self {
            SymbolId::MulticlassId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_defm_id(&self) -> Option<DefmId> {
        match self {
            SymbolId::DefmId(id) => Some(*id),
            _ => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Symbol<'a> {
    Class(&'a Class),
    Def(&'a Def),
    TemplateArgument(&'a TemplateArgument),
    RecordField(&'a RecordField),
    Variable(&'a Variable),
    Defset(&'a Defset),
    Multiclass(&'a Multiclass),
    Defm(&'a Defm),
}

impl<'a> Symbol<'a> {
    pub fn name(&self) -> &EcoString {
        match self {
            Self::Class(class) => class.name(),
            Self::Def(def) => def.name(),
            Self::TemplateArgument(template_argument) => &template_argument.name,
            Self::RecordField(record_field) => &record_field.name,
            Self::Variable(variable) => &variable.name,
            Self::Defset(defset) => &defset.name,
            Self::Multiclass(multiclass) => &multiclass.name,
            Self::Defm(defm) => &defm.name,
        }
    }

    pub fn define_loc(&self) -> &FileRange {
        match self {
            Self::Class(class) => class.define_loc(),
            Self::Def(def) => def.define_loc(),
            Self::TemplateArgument(template_argument) => &template_argument.define_loc,
            Self::RecordField(record_field) => &record_field.define_loc,
            Self::Variable(variable) => &variable.define_loc,
            Self::Defset(defset) => &defset.define_loc,
            Self::Multiclass(multiclass) => &multiclass.define_loc,
            Self::Defm(defm) => &defm.define_loc,
        }
    }

    pub fn reference_locs(&self) -> &[FileRange] {
        match self {
            Self::Class(class) => class.reference_locs(),
            Self::Def(def) => def.reference_locs(),
            Self::TemplateArgument(template_argument) => &template_argument.reference_locs,
            Self::RecordField(record_field) => &record_field.reference_locs,
            Self::Variable(variable) => &variable.reference_locs,
            Self::Defset(defset) => &defset.reference_locs,
            Self::Multiclass(multiclass) => &multiclass.reference_locs,
            Self::Defm(defm) => &defm.reference_locs,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum SymbolMut<'a> {
    Class(&'a mut Class),
    Def(&'a mut Def),
    TemplateArgument(&'a mut TemplateArgument),
    RecordField(&'a mut RecordField),
    Variable(&'a mut Variable),
    Defset(&'a mut Defset),
    Multiclass(&'a mut Multiclass),
    Defm(&'a mut Defm),
}

impl<'a> SymbolMut<'a> {
    pub fn add_reference(&mut self, reference_loc: FileRange) {
        match self {
            Self::Class(class) => class.add_reference_loc(reference_loc),
            Self::Def(def) => def.add_reference_loc(reference_loc),
            Self::TemplateArgument(template_argument) => {
                template_argument.reference_locs.push(reference_loc)
            }
            Self::RecordField(record_field) => record_field.reference_locs.push(reference_loc),
            Self::Variable(variable) => variable.reference_locs.push(reference_loc),
            Self::Defset(defset) => defset.reference_locs.push(reference_loc),
            Self::Multiclass(multiclass) => multiclass.reference_locs.push(reference_loc),
            Self::Defm(defm) => defm.reference_locs.push(reference_loc),
        }
    }
}
