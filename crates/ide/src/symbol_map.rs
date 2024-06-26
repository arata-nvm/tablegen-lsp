use std::fmt::Debug;
use std::ops::DerefMut;
use std::{collections::HashMap, ops::Deref};

use ecow::EcoString;
use id_arena::Arena;

use syntax::parser::{TextRange, TextSize};

use crate::file_system::{FileId, FilePosition, FileRange};

use self::variable::{Variable, VariableId};
use self::{
    class::{Class, ClassId},
    def::{Def, DefId},
    field::{Field, FieldId},
    record::Record,
    symbol::{Symbol, SymbolId, SymbolMut},
    template_arg::{TemplateArgument, TemplateArgumentId},
};

pub mod class;
pub mod def;
pub mod expr;
pub mod field;
pub mod record;
pub mod symbol;
pub mod template_arg;
pub mod typ;
pub mod value;
pub mod variable;

#[derive(Debug, Default, Eq, PartialEq)]
pub struct SymbolMap {
    class_list: Arena<Class>,
    template_arg_list: Arena<TemplateArgument>,
    field_list: Arena<Field>,
    def_list: Arena<Def>,
    variable_list: Arena<Variable>,

    name_to_class: HashMap<EcoString, ClassId>,
    name_to_def: HashMap<EcoString, DefId>,

    file_to_symbol_list: HashMap<FileId, Vec<SymbolId>>,
    pos_to_symbol_map: HashMap<FileId, IntervalMap<TextSize, SymbolId>>,
}

// immutable api
impl SymbolMap {
    pub fn class(&self, class_id: ClassId) -> &Class {
        self.class_list.get(class_id).expect("invalid class id")
    }

    pub fn class_mut(&mut self, class_id: ClassId) -> &mut Class {
        self.class_list.get_mut(class_id).expect("invalid class id")
    }

    pub fn find_class(&self, name: &EcoString) -> Option<ClassId> {
        self.name_to_class.get(name).copied()
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

    pub fn field(&self, field_id: FieldId) -> &Field {
        self.field_list.get(field_id).expect("invalid field id")
    }

    pub fn field_mut(&mut self, field_id: FieldId) -> &mut Field {
        self.field_list.get_mut(field_id).expect("invalid field id")
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

    pub fn symbol(&self, id: SymbolId) -> Symbol {
        match id {
            SymbolId::ClassId(class_id) => Symbol::Class(self.class(class_id)),
            SymbolId::TemplateArgumentId(template_arg_id) => {
                Symbol::TemplateArgument(self.template_arg(template_arg_id))
            }
            SymbolId::FieldId(field_id) => Symbol::Field(self.field(field_id)),
            SymbolId::DefId(def_id) => Symbol::Def(self.def(def_id)),
            SymbolId::VariableId(variable_id) => Symbol::Variable(self.variable(variable_id)),
        }
    }

    pub fn symbol_mut(&mut self, id: SymbolId) -> SymbolMut {
        match id {
            SymbolId::ClassId(class_id) => SymbolMut::Class(self.class_mut(class_id)),
            SymbolId::TemplateArgumentId(template_arg_id) => {
                SymbolMut::TemplateArgument(self.template_arg_mut(template_arg_id))
            }
            SymbolId::FieldId(field_id) => SymbolMut::Field(self.field_mut(field_id)),
            SymbolId::DefId(def_id) => SymbolMut::Def(self.def_mut(def_id)),
            SymbolId::VariableId(variable_id) => {
                SymbolMut::Variable(self.variable_mut(variable_id))
            }
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

    pub fn find_symbol_at(&self, pos: FilePosition) -> Option<Symbol> {
        let id = self
            .pos_to_symbol_map
            .get(&pos.file)
            .and_then(|map| map.values_overlap(pos.position).next().cloned())?;
        Some(self.symbol(id))
    }
}

// mutable api
impl SymbolMap {
    pub fn add_class(&mut self, record: Record) -> ClassId {
        let class = record.into_class();
        let name = class.name.clone();
        let define_loc = class.define_loc;
        let id = self.class_list.alloc(class);
        self.name_to_class.insert(name, id);
        self.file_to_symbol_list
            .entry(define_loc.file)
            .or_default()
            .push(id.into());
        self.add_to_pos_to_symbol_map(define_loc, id);
        id
    }

    pub fn replace_class(&mut self, id: ClassId, record: Record) {
        let class_ref = self.class_mut(id);

        let mut new_class = record.into_class();
        new_class
            .reference_locs
            .clone_from(&class_ref.reference_locs);

        assert!(class_ref.name == new_class.name);
        let _ = std::mem::replace(class_ref, new_class);
    }

    pub fn add_template_argument(&mut self, template_arg: TemplateArgument) -> TemplateArgumentId {
        let define_loc = template_arg.define_loc;
        let id = self.template_arg_list.alloc(template_arg);
        self.add_to_pos_to_symbol_map(define_loc, id);
        id
    }

    pub fn add_field(&mut self, field: Field) -> FieldId {
        let define_loc = field.define_loc;
        let id = self.field_list.alloc(field);
        self.add_to_pos_to_symbol_map(define_loc, id);
        id
    }

    pub fn add_def(&mut self, def: Def) -> DefId {
        let name = def.name.clone();
        let define_loc = def.define_loc;
        let id = self.def_list.alloc(def);
        self.name_to_def.insert(name, id);
        self.file_to_symbol_list
            .entry(define_loc.file)
            .or_default()
            .push(id.into());
        self.add_to_pos_to_symbol_map(define_loc, id);
        id
    }

    pub fn replace_def(&mut self, id: DefId, mut new_def: Def) {
        let def_ref = self.def_mut(id);

        new_def.reference_locs.clone_from(&def_ref.reference_locs);

        assert!(def_ref.name == new_def.name);
        let _ = std::mem::replace(def_ref, new_def);
    }

    pub fn add_reference(&mut self, symbol_id: impl Into<SymbolId>, reference_loc: FileRange) {
        let symbol_id = symbol_id.into();
        let mut symbol = self.symbol_mut(symbol_id);
        symbol.add_reference(reference_loc);
        self.add_to_pos_to_symbol_map(reference_loc, symbol_id);
    }

    pub fn add_variable(&mut self, variable: Variable) -> VariableId {
        let define_loc = variable.define_loc;
        let id = self.variable_list.alloc(variable);
        self.file_to_symbol_list
            .entry(define_loc.file)
            .or_default()
            .push(id.into());
        self.add_to_pos_to_symbol_map(define_loc, id);
        id
    }

    fn add_to_pos_to_symbol_map(&mut self, loc: FileRange, symbol_id: impl Into<SymbolId>) {
        // ignore the symbol if its range is empty
        if loc.range.is_empty() {
            return;
        }

        self.pos_to_symbol_map
            .entry(loc.file)
            .or_insert_with(IntervalMap::new)
            .insert(loc.range.into(), symbol_id.into());
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
