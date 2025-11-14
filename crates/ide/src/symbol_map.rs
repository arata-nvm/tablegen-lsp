use std::fmt::Debug;
use std::ops::DerefMut;
use std::{collections::HashMap, ops::Deref};

use class::{Class, ClassId};
use def::{Def, DefId};
use defm::{Defm, DefmId};
use defset::{Defset, DefsetId};
use ecow::EcoString;
use id_arena::Arena;

use multiclass::{Multiclass, MulticlassId};
use record::{Record, RecordField, RecordFieldId, RecordId, RecordMut};
use symbol::{Symbol, SymbolId, SymbolMut};
use syntax::parser::{TextRange, TextSize};
use template_arg::{TemplateArgument, TemplateArgumentId};
use thiserror::Error;
use variable::{Variable, VariableId};

use crate::file_system::{FileId, FilePosition, FileRange};

pub mod class;
pub mod def;
pub mod defm;
pub mod defset;
pub mod multiclass;
pub mod record;
pub mod symbol;
pub mod template_arg;
pub mod typ;
pub mod variable;

#[derive(Debug, Default, Eq, PartialEq)]
pub struct SymbolMap {
    class_list: Arena<Class>,
    def_list: Arena<Def>,
    template_arg_list: Arena<TemplateArgument>,
    record_field_list: Arena<RecordField>,
    variable_list: Arena<Variable>,
    defset_list: Arena<Defset>,
    multiclass_list: Arena<Multiclass>,
    defm_list: Arena<Defm>,

    name_to_class: HashMap<EcoString, ClassId>,
    name_to_class_declaration: HashMap<EcoString, ClassId>,
    name_to_def: HashMap<EcoString, DefId>,
    name_to_defset: HashMap<EcoString, DefsetId>,
    name_to_multiclass: HashMap<EcoString, MulticlassId>,
    file_to_symbol_list: HashMap<FileId, Vec<SymbolId>>,
    pos_to_symbol_map: HashMap<FileId, IntervalMap<TextSize, SymbolId>>,
}

#[derive(Debug, Error)]
pub enum SymbolMapError {
    #[error("Class '{0}' already defined")]
    ClassAlreadyDefined(EcoString),
    #[error("Class '{0}' has cyclic inheritance")]
    ClassCyclicInheritance(EcoString),
}

// immutable api
impl SymbolMap {
    pub fn class(&self, class_id: ClassId) -> &Class {
        self.class_list.get(class_id).expect("invalid record id")
    }

    pub fn class_mut(&mut self, class_id: ClassId) -> &mut Class {
        self.class_list.get_mut(class_id).expect("invalid class id")
    }

    pub fn find_class(&self, name: &EcoString) -> Option<ClassId> {
        self.name_to_class.get(name).copied()
    }

    pub fn iter_class(&self) -> impl Iterator<Item = ClassId> + '_ {
        self.name_to_class.values().copied()
    }

    pub fn def(&self, def_id: DefId) -> &Def {
        self.def_list.get(def_id).expect("invalid def id")
    }

    pub fn def_mut(&mut self, def_id: DefId) -> &mut Def {
        self.def_list.get_mut(def_id).expect("invalid def id")
    }

    pub fn find_def(&self, name: &EcoString) -> Option<DefId> {
        self.name_to_def.get(name).copied()
    }

    pub fn iter_def(&self) -> impl Iterator<Item = DefId> + '_ {
        self.name_to_def.values().copied()
    }

    pub fn record(&self, record_id: RecordId) -> Record<'_> {
        match record_id {
            RecordId::Class(class_id) => Record::Class(self.class(class_id)),
            RecordId::Def(def_id) => Record::Def(self.def(def_id)),
        }
    }

    pub fn record_mut(&mut self, record_id: RecordId) -> RecordMut<'_> {
        match record_id {
            RecordId::Class(class_id) => RecordMut::Class(self.class_mut(class_id)),
            RecordId::Def(def_id) => RecordMut::Def(self.def_mut(def_id)),
        }
    }

    pub fn template_arg(&self, template_arg_id: TemplateArgumentId) -> &TemplateArgument {
        self.template_arg_list
            .get(template_arg_id)
            .expect("invalid template argument id")
    }

    pub fn template_arg_mut(
        &mut self,
        template_arg_id: TemplateArgumentId,
    ) -> &mut TemplateArgument {
        self.template_arg_list
            .get_mut(template_arg_id)
            .expect("invalid template argument id")
    }

    pub fn record_field(&self, record_field_id: RecordFieldId) -> &RecordField {
        self.record_field_list
            .get(record_field_id)
            .expect("invalid record field id")
    }

    pub fn record_field_mut(&mut self, record_field_id: RecordFieldId) -> &mut RecordField {
        self.record_field_list
            .get_mut(record_field_id)
            .expect("invalid record field id")
    }

    pub fn variable(&self, variable_id: VariableId) -> &Variable {
        self.variable_list
            .get(variable_id)
            .expect("invalid variable id")
    }

    pub fn variable_mut(&mut self, variable_id: VariableId) -> &mut Variable {
        self.variable_list
            .get_mut(variable_id)
            .expect("invalid variable id")
    }

    pub fn defset(&self, defset_id: DefsetId) -> &Defset {
        self.defset_list.get(defset_id).expect("invalid defset id")
    }

    pub fn defset_mut(&mut self, defset_id: DefsetId) -> &mut Defset {
        self.defset_list
            .get_mut(defset_id)
            .expect("invalid defset id")
    }

    pub fn find_defset(&self, name: &EcoString) -> Option<DefsetId> {
        self.name_to_defset.get(name).copied()
    }

    pub fn iter_defset(&self) -> impl Iterator<Item = DefsetId> + '_ {
        self.name_to_defset.values().copied()
    }

    pub fn multiclass(&self, multiclass_id: MulticlassId) -> &Multiclass {
        self.multiclass_list
            .get(multiclass_id)
            .expect("invalid multiclass id")
    }

    pub fn multiclass_mut(&mut self, multiclass_id: MulticlassId) -> &mut Multiclass {
        self.multiclass_list
            .get_mut(multiclass_id)
            .expect("invalid multiclass id")
    }

    pub fn find_multiclass(&self, name: &EcoString) -> Option<MulticlassId> {
        self.name_to_multiclass.get(name).copied()
    }

    pub fn defm(&self, defm_id: DefmId) -> &Defm {
        self.defm_list.get(defm_id).expect("invalid defm id")
    }

    pub fn defm_mut(&mut self, defm_id: DefmId) -> &mut Defm {
        self.defm_list.get_mut(defm_id).expect("invalid defm id")
    }

    pub fn symbol(&self, id: SymbolId) -> Symbol<'_> {
        match id {
            SymbolId::ClassId(class_id) => Symbol::Class(self.class(class_id)),
            SymbolId::DefId(def_id) => Symbol::Def(self.def(def_id)),
            SymbolId::TemplateArgumentId(template_arg_id) => {
                Symbol::TemplateArgument(self.template_arg(template_arg_id))
            }
            SymbolId::RecordFieldId(record_field_id) => {
                Symbol::RecordField(self.record_field(record_field_id))
            }
            SymbolId::VariableId(variable_id) => Symbol::Variable(self.variable(variable_id)),
            SymbolId::DefsetId(defset_id) => Symbol::Defset(self.defset(defset_id)),
            SymbolId::MulticlassId(multiclass_id) => {
                Symbol::Multiclass(self.multiclass(multiclass_id))
            }
            SymbolId::DefmId(defm_id) => Symbol::Defm(self.defm(defm_id)),
        }
    }

    pub fn symbol_mut(&mut self, id: SymbolId) -> SymbolMut<'_> {
        match id {
            SymbolId::ClassId(class_id) => SymbolMut::Class(self.class_mut(class_id)),
            SymbolId::DefId(def_id) => SymbolMut::Def(self.def_mut(def_id)),
            SymbolId::TemplateArgumentId(template_arg_id) => {
                SymbolMut::TemplateArgument(self.template_arg_mut(template_arg_id))
            }
            SymbolId::RecordFieldId(record_field_id) => {
                SymbolMut::RecordField(self.record_field_mut(record_field_id))
            }
            SymbolId::VariableId(variable_id) => {
                SymbolMut::Variable(self.variable_mut(variable_id))
            }
            SymbolId::DefsetId(defset_id) => SymbolMut::Defset(self.defset_mut(defset_id)),
            SymbolId::MulticlassId(multiclass_id) => {
                SymbolMut::Multiclass(self.multiclass_mut(multiclass_id))
            }
            SymbolId::DefmId(defm_id) => SymbolMut::Defm(self.defm_mut(defm_id)),
        }
    }

    pub fn iter_symbols_in_file(&self, file_id: FileId) -> Option<impl Iterator<Item = SymbolId>> {
        self.file_to_symbol_list
            .get(&file_id)
            .cloned()
            .map(|class_list| class_list.into_iter())
    }

    pub fn iter_symbols_in_range(
        &self,
        loc: FileRange,
    ) -> Option<impl Iterator<Item = (FileRange, SymbolId)> + '_> {
        let map = self.pos_to_symbol_map.get(&loc.file)?;
        Some(map.iter(loc.range).map(move |(range, id)| {
            (
                FileRange::new(loc.file, TextRange::new(range.start, range.end)),
                *id,
            )
        }))
    }

    pub fn find_symbol_at(&self, pos: FilePosition) -> Option<Symbol<'_>> {
        let id = self
            .pos_to_symbol_map
            .get(&pos.file)
            .and_then(|map| map.values_overlap(pos.position).next().cloned())?;
        Some(self.symbol(id))
    }
}

// mutable api
impl SymbolMap {
    pub fn add_class(&mut self, class: Class) -> Result<ClassId, SymbolMapError> {
        let name = class.name.clone();
        let define_loc = class.define_loc;
        let id = self.alloc_or_replace_class(class)?;
        self.name_to_class.insert(name, id);
        self.add_symbol_to_file(id, define_loc, true);
        Ok(id)
    }

    fn alloc_or_replace_class(&mut self, class: Class) -> Result<ClassId, SymbolMapError> {
        let Some(class_id) = self.name_to_class.get(&class.name).copied() else {
            return Ok(self.class_list.alloc(class));
        };

        let existing_class = self.class(class_id);
        if !existing_class.is_empty() {
            return Err(SymbolMapError::ClassAlreadyDefined(class.name.clone()));
        }

        let class_mut = self.class_mut(class_id);
        let _ = std::mem::replace(class_mut, class);
        Ok(class_id)
    }

    pub fn add_def(&mut self, def: Def, is_global: bool) -> DefId {
        let name = def.name.clone();
        let define_loc = def.define_loc;
        let id = self.def_list.alloc(def);
        self.name_to_def.insert(name, id);
        self.add_symbol_to_file(id, define_loc, is_global);
        id
    }

    pub fn add_temporary_def(&mut self, def: Def) -> DefId {
        self.def_list.alloc(def)
    }

    pub fn add_template_argument(&mut self, template_arg: TemplateArgument) -> TemplateArgumentId {
        let define_loc = template_arg.define_loc;
        let id = self.template_arg_list.alloc(template_arg);
        self.add_symbol_to_file(id, define_loc, false);
        id
    }

    pub fn add_record_field(&mut self, record_field: RecordField) -> RecordFieldId {
        let define_loc = record_field.define_loc;
        let id = self.record_field_list.alloc(record_field);
        self.add_symbol_to_file(id, define_loc, false);
        id
    }

    pub fn add_variable(&mut self, variable: Variable) -> VariableId {
        let define_loc = variable.define_loc;
        let id = self.variable_list.alloc(variable);
        self.add_symbol_to_file(id, define_loc, true);
        id
    }

    pub fn add_defset(&mut self, defset: Defset) -> DefsetId {
        let name = defset.name.clone();
        let define_loc = defset.define_loc;
        let id = self.defset_list.alloc(defset);
        self.name_to_defset.insert(name, id);
        self.add_symbol_to_file(id, define_loc, true);
        id
    }

    pub fn add_multiclass(&mut self, multiclass: Multiclass) -> MulticlassId {
        let name = multiclass.name.clone();
        let define_loc = multiclass.define_loc;
        let id = self.multiclass_list.alloc(multiclass);
        self.name_to_multiclass.insert(name, id);
        self.add_symbol_to_file(id, define_loc, true);
        id
    }

    pub fn add_defm(&mut self, defm: Defm, is_global: bool) -> DefmId {
        let define_loc = defm.define_loc;
        let id = self.defm_list.alloc(defm);
        self.add_symbol_to_file(id, define_loc, is_global);
        id
    }

    pub fn add_temporary_defm(&mut self, defm: Defm) -> DefmId {
        self.defm_list.alloc(defm)
    }

    pub fn add_reference(&mut self, symbol_id: impl Into<SymbolId>, reference_loc: FileRange) {
        let symbol_id = symbol_id.into();
        let mut symbol = self.symbol_mut(symbol_id);
        symbol.add_reference(reference_loc);
        self.add_symbol_to_file(symbol_id, reference_loc, false);
    }

    fn add_symbol_to_file(&mut self, id: impl Into<SymbolId>, loc: FileRange, is_global: bool) {
        let id = id.into();

        if is_global {
            self.file_to_symbol_list
                .entry(loc.file)
                .or_default()
                .push(id);
        }

        if !loc.range.is_empty() {
            self.pos_to_symbol_map
                .entry(loc.file)
                .or_insert_with(IntervalMap::new)
                .insert(loc.range.into(), id);
        }
    }

    pub fn add_parent_to_record(
        &mut self,
        record_id: RecordId,
        parent_id: ClassId,
    ) -> Result<(), SymbolMapError> {
        if let RecordId::Class(class_id) = record_id {
            let class = self.class(class_id);
            if class_id == parent_id || class.is_subclass_of(self, parent_id) {
                return Err(SymbolMapError::ClassCyclicInheritance(class.name.clone()));
            }
        }

        let mut record = self.record_mut(record_id);
        record.add_parent(parent_id);
        Ok(())
    }
}

// FIXME
// because iset::IntervalMap does not implement PartialEq and Eq, we have to implement them
#[derive(Debug, Default)]
struct IntervalMap<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq>(
    iset::IntervalMap<T, V>,
);

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> IntervalMap<T, V> {
    fn new() -> Self {
        Self(iset::IntervalMap::new())
    }
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> Deref for IntervalMap<T, V> {
    type Target = iset::IntervalMap<T, V>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> DerefMut for IntervalMap<T, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> PartialEq for IntervalMap<T, V> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }

        for ((range1, value1), (range2, value2)) in self.0.iter(..).zip(other.0.iter(..)) {
            if range1 != range2 || value1 != value2 {
                return false;
            }
        }
        true
    }
}

impl<T: PartialOrd + Copy + Debug, V: Debug + PartialEq + Eq> Eq for IntervalMap<T, V> {}
