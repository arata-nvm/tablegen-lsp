use std::collections::BTreeMap;
use std::fmt::Debug;
use std::ops::DerefMut;
use std::{collections::HashMap, ops::Deref};

use ecow::EcoString;
use id_arena::{Arena, Id};

use syntax::parser::{TextRange, TextSize};
use thiserror::Error;

use crate::file_system::{FileId, FilePosition, FileRange};

#[derive(Debug, Eq, PartialEq)]
pub struct Class {
    pub name: EcoString,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
    pub name_to_template_arg: BTreeMap<EcoString, TemplateArgumentId>,
    pub name_to_field: BTreeMap<EcoString, FieldId>,
    pub parent_class_list: Vec<ClassId>,
}

pub type ClassId = Id<Class>;

impl From<ClassId> for SymbolId {
    fn from(id: ClassId) -> Self {
        SymbolId::ClassId(id)
    }
}

#[derive(Debug, Error)]
pub enum ClassError {
    #[error("unknown field: {0}")]
    UnknownField(EcoString),
    #[error("type of field '{0}' is incompatible: '{1}' and '{2}'")]
    IncompatibleType(EcoString, Type, Type),
}

impl Class {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            name,
            define_loc,
            reference_locs: Vec::new(),
            name_to_template_arg: BTreeMap::new(),
            name_to_field: BTreeMap::new(),
            parent_class_list: Vec::new(),
        }
    }

    pub fn add_template_arg(
        &mut self,
        symbol_map: &mut SymbolMap,
        template_arg: TemplateArgument,
    ) -> TemplateArgumentId {
        let name = template_arg.name.clone();
        let id = symbol_map.add_template_argument(template_arg);
        self.name_to_template_arg.insert(name, id);
        id
    }

    pub fn iter_template_arg(&self) -> impl Iterator<Item = TemplateArgumentId> + '_ {
        self.name_to_template_arg.values().copied()
    }

    pub fn add_field(
        &mut self,
        symbol_map: &mut SymbolMap,
        new_field: Field,
    ) -> Result<FieldId, (TextRange, ClassError)> {
        if let Some(old_field_id) = self.name_to_field.get(&new_field.name) {
            let old_field = symbol_map.field(*old_field_id);
            if old_field.typ != new_field.typ {
                return Err((
                    new_field.define_loc.range,
                    ClassError::IncompatibleType(
                        new_field.name.clone(),
                        old_field.typ.clone(),
                        new_field.typ.clone(),
                    ),
                ));
            }
        }

        let name = new_field.name.clone();
        let id = symbol_map.add_field(new_field);
        self.name_to_field.insert(name, id);
        Ok(id)
    }

    pub fn modify_field(
        &mut self,
        symbol_map: &mut SymbolMap,
        name: EcoString,
        value: Expr,
        define_loc: FileRange,
    ) -> Result<FieldId, ClassError> {
        match self.name_to_field.get(&name) {
            Some(old_field_id) => {
                let old_field = symbol_map.field(*old_field_id);
                let new_field = Field::new(name.clone(), old_field.typ.clone(), value, define_loc);
                let id = symbol_map.add_field(new_field);
                self.name_to_field.insert(name, id);
                Ok(id)
            }
            None => Err(ClassError::UnknownField(name)),
        }
    }

    pub fn iter_field(&self) -> impl Iterator<Item = FieldId> + '_ {
        self.name_to_field.values().copied()
    }

    pub fn inherit(
        &mut self,
        symbol_map: &mut SymbolMap,
        parent_class_id: ClassId,
        reference_loc: FileRange,
    ) {
        symbol_map.add_reference(parent_class_id, reference_loc);
        self.parent_class_list.push(parent_class_id);

        let parent_class = symbol_map.class(parent_class_id);
        self.name_to_field
            .extend(parent_class.name_to_field.clone());
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct TemplateArgument {
    pub name: EcoString,
    pub typ: Type,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

pub type TemplateArgumentId = Id<TemplateArgument>;

impl From<TemplateArgumentId> for SymbolId {
    fn from(id: TemplateArgumentId) -> Self {
        SymbolId::TemplateArgumentId(id)
    }
}

impl TemplateArgument {
    pub fn new(name: EcoString, typ: Type, define_loc: FileRange) -> Self {
        Self {
            name,
            typ,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Field {
    pub name: EcoString,
    pub typ: Type,
    pub value: Expr,
    pub define_loc: FileRange,
    pub reference_locs: Vec<FileRange>,
}

pub type FieldId = Id<Field>;

impl From<FieldId> for SymbolId {
    fn from(id: FieldId) -> Self {
        SymbolId::FieldId(id)
    }
}

impl Field {
    pub fn new(name: EcoString, typ: Type, value: Expr, define_loc: FileRange) -> Self {
        Self {
            name,
            typ,
            value,
            define_loc,
            reference_locs: Vec::new(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Simple(SimpleExpr),
    // RangeSuffix(Box<Expr>, Vec<Range>),
    // SliceSuffix(Box<Expr>, Vec<Slice>),
    // FieldSuffix(Box<Expr>, EcoString),
    // Paste(Box<Expr>, SimpleExpr),
}

// #[derive(Debug, Eq, PartialEq)]
// pub struct Range {
//     start: i64,
//     end: i64,
// }

// #[derive(Debug, Eq, PartialEq)]
// pub struct Slice {
//     start: Expr,
//     end: Expr,
// }

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum SimpleExpr {
    Integer(i64),
    String(String),
    Code(EcoString),
    Boolean(bool),
    Uninitialized,
    Bits(Vec<Expr>),
    List(Vec<Expr>),
    // Dag(DagArg, Vec<DagArg>),
    Identifier((SymbolId, EcoString)),
    // ClassValue,
    // BangOperator,
    // CondOperator,
}

impl std::fmt::Display for SimpleExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Uninitialized => write!(f, "?"),
            Self::Integer(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "\"{value}\""),
            Self::Code(value) => write!(f, "[{{ {value} }}]"),
            Self::Boolean(value) => write!(f, "{value}"),
            Self::Bits(values) => {
                write!(
                    f,
                    "{{ {} }}",
                    values
                        .iter()
                        .map(|it| it.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::List(values) => {
                write!(
                    f,
                    "[ {} ]",
                    values
                        .iter()
                        .map(|it| it.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::Identifier((_, name)) => write!(f, "{name}"),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct DagArg {
    value: Expr,
    var_name: EcoString,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash, PartialOrd, Ord)]
pub enum SymbolId {
    ClassId(ClassId),
    TemplateArgumentId(TemplateArgumentId),
    FieldId(FieldId),
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
}

#[derive(Debug, Eq, PartialEq)]
pub enum Symbol<'a> {
    Class(&'a Class),
    TemplateArgument(&'a TemplateArgument),
    Field(&'a Field),
}

impl<'a> Symbol<'a> {
    pub fn name(&self) -> &EcoString {
        match self {
            Self::Class(class) => &class.name,
            Self::TemplateArgument(template_arg) => &template_arg.name,
            Self::Field(field) => &field.name,
        }
    }

    pub fn define_loc(&self) -> &FileRange {
        match self {
            Self::Class(class) => &class.define_loc,
            Self::TemplateArgument(template_arg) => &template_arg.define_loc,
            Self::Field(field) => &field.define_loc,
        }
    }

    pub fn reference_locs(&self) -> &[FileRange] {
        match self {
            Self::Class(class) => &class.reference_locs,
            Self::TemplateArgument(template_arg) => &template_arg.reference_locs,
            Self::Field(field) => &field.reference_locs,
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
}

#[derive(Debug, Eq, PartialEq)]
pub enum SymbolMut<'a> {
    Class(&'a mut Class),
    TemplateArgument(&'a mut TemplateArgument),
    Field(&'a mut Field),
}

impl<'a> SymbolMut<'a> {
    pub fn add_reference(&mut self, reference_loc: FileRange) {
        match self {
            Self::Class(class) => class.reference_locs.push(reference_loc),
            Self::TemplateArgument(template_arg) => template_arg.reference_locs.push(reference_loc),
            Self::Field(field) => field.reference_locs.push(reference_loc),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Bit,
    Int,
    String,
    Dag,
    Bits(i64),
    List(Box<Type>),
    Class((ClassId, EcoString)),
    Code,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bit => write!(f, "bit"),
            Self::Int => write!(f, "int"),
            Self::String => write!(f, "string"),
            Self::Dag => write!(f, "dag"),
            Self::Bits(width) => write!(f, "bits<{}>", width),
            Self::List(typ) => write!(f, "list<{}>", typ),
            Self::Class((_, name)) => write!(f, "{}", name),
            Self::Code => write!(f, "code"),
        }
    }
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct SymbolMap {
    class_list: Arena<Class>,
    template_arg_list: Arena<TemplateArgument>,
    field_list: Arena<Field>,
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

    pub fn symbol(&self, id: SymbolId) -> Symbol {
        match id {
            SymbolId::ClassId(class_id) => Symbol::Class(self.class(class_id)),
            SymbolId::TemplateArgumentId(template_arg_id) => {
                Symbol::TemplateArgument(self.template_arg(template_arg_id))
            }
            SymbolId::FieldId(field_id) => Symbol::Field(self.field(field_id)),
        }
    }

    pub fn symbol_mut(&mut self, id: SymbolId) -> SymbolMut {
        match id {
            SymbolId::ClassId(class_id) => SymbolMut::Class(self.class_mut(class_id)),
            SymbolId::TemplateArgumentId(template_arg_id) => {
                SymbolMut::TemplateArgument(self.template_arg_mut(template_arg_id))
            }
            SymbolId::FieldId(field_id) => SymbolMut::Field(self.field_mut(field_id)),
        }
    }

    pub fn iter_symbol_in(&self, file_id: FileId) -> Option<impl Iterator<Item = SymbolId>> {
        self.file_to_symbol_list
            .get(&file_id)
            .cloned()
            .map(|class_list| class_list.into_iter())
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
    pub fn add_class(&mut self, class: Class) -> ClassId {
        let define_loc = class.define_loc;
        let id = self.class_list.alloc(class);
        self.file_to_symbol_list
            .entry(define_loc.file)
            .or_default()
            .push(id.into());
        self.pos_to_symbol_map
            .entry(define_loc.file)
            .or_insert_with(IntervalMap::new)
            .insert(define_loc.range.into(), id.into());
        id
    }

    pub fn add_template_argument(&mut self, template_arg: TemplateArgument) -> TemplateArgumentId {
        let define_loc = template_arg.define_loc;
        let id = self.template_arg_list.alloc(template_arg);
        self.pos_to_symbol_map
            .entry(define_loc.file)
            .or_insert_with(IntervalMap::new)
            .insert(define_loc.range.into(), SymbolId::TemplateArgumentId(id));
        id
    }

    pub fn add_field(&mut self, field: Field) -> FieldId {
        let define_loc = field.define_loc;
        let id = self.field_list.alloc(field);
        self.pos_to_symbol_map
            .entry(define_loc.file)
            .or_insert_with(IntervalMap::new)
            .insert(define_loc.range.into(), SymbolId::FieldId(id));
        id
    }

    pub fn add_reference(&mut self, symbol_id: impl Into<SymbolId>, reference_loc: FileRange) {
        let symbol_id = symbol_id.into();
        let mut symbol = self.symbol_mut(symbol_id);
        symbol.add_reference(reference_loc);
        self.pos_to_symbol_map
            .entry(reference_loc.file)
            .or_insert_with(IntervalMap::new)
            .insert(reference_loc.range.into(), symbol_id);
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
