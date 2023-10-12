use std::{collections::HashMap, fmt};

use ecow::EcoString;
use id_arena::Id;
use tablegen_parser::error::Range;

use crate::document::DocumentId;

pub type Location = (DocumentId, Range);

pub type SymbolId = Id<Symbol>;

#[derive(Debug)]
pub enum Symbol {
    Record(Record),
    RecordField(RecordField),
}

impl Symbol {
    pub fn name(&self) -> &EcoString {
        match self {
            Self::Record(record) => record.name(),
            Self::RecordField(field) => field.name(),
        }
    }

    pub fn define_loc(&self) -> &Location {
        match self {
            Self::Record(record) => record.define_loc(),
            Self::RecordField(field) => field.define_loc(),
        }
    }

    pub fn add_reference(&mut self, loc: Location) {
        match self {
            Self::Record(record) => record.add_reference(loc),
            Self::RecordField(field) => field.add_reference(loc),
        }
    }

    pub fn reference_locs(&self) -> &[Location] {
        match self {
            Self::Record(record) => record.reference_locs(),
            Self::RecordField(field) => field.reference_locs(),
        }
    }

    pub fn as_record(&self) -> &Record {
        match self {
            Self::Record(record) => record,
            Self::RecordField(_) => panic!(),
        }
    }

    pub fn as_record_mut(&mut self) -> &mut Record {
        match self {
            Self::Record(record) => record,
            Self::RecordField(_) => panic!(),
        }
    }

    pub fn as_field(&self) -> &RecordField {
        match self {
            Self::Record(_) => panic!(),
            Self::RecordField(field) => field,
        }
    }

    pub fn as_field_mut(&mut self) -> &mut RecordField {
        match self {
            Self::Record(_) => panic!(),
            Self::RecordField(field) => field,
        }
    }
}

#[derive(Debug)]
pub struct Record {
    name: EcoString,
    define_loc: Location,
    reference_locs: Vec<Location>,
    template_args: HashMap<EcoString, SymbolId>,
    fields: HashMap<EcoString, SymbolId>,
    kind: RecordKind,
}

#[derive(Debug, Clone)]
pub enum RecordKind {
    Class,
    Def,
}

impl Record {
    pub fn new(name: EcoString, define_loc: Location, kind: RecordKind) -> Self {
        Self {
            name,
            define_loc,
            reference_locs: Vec::new(),
            template_args: HashMap::new(),
            fields: HashMap::new(),
            kind,
        }
    }

    pub fn name(&self) -> &EcoString {
        &self.name
    }

    pub fn define_loc(&self) -> &Location {
        &self.define_loc
    }

    pub fn add_reference(&mut self, loc: Location) {
        self.reference_locs.push(loc);
    }

    pub fn reference_locs(&self) -> &[Location] {
        &self.reference_locs
    }

    pub fn add_template_arg(&mut self, name: EcoString, template_arg_id: SymbolId) {
        self.template_args.insert(name, template_arg_id);
    }

    pub fn template_args(&self) -> Vec<&SymbolId> {
        self.template_args.values().collect()
    }

    pub fn add_field(&mut self, name: EcoString, field_id: SymbolId) {
        self.fields.insert(name, field_id);
    }

    pub fn fields(&self) -> Vec<&SymbolId> {
        self.fields.values().collect()
    }

    pub fn find_field(&self, name: &EcoString) -> Option<&SymbolId> {
        self.fields.get(name)
    }

    pub fn kind(&self) -> RecordKind {
        self.kind.clone()
    }
}

#[derive(Debug)]
pub struct RecordField {
    name: EcoString,
    define_loc: Location,
    reference_locs: Vec<Location>,
    kind: RecordFieldKind,
    typ: RecordFieldType,
}

impl RecordField {
    pub fn new(
        name: EcoString,
        define_loc: Location,
        kind: RecordFieldKind,
        typ: RecordFieldType,
    ) -> Self {
        Self {
            name,
            define_loc,
            reference_locs: Vec::new(),
            kind,
            typ,
        }
    }

    pub fn name(&self) -> &EcoString {
        &self.name
    }

    pub fn define_loc(&self) -> &Location {
        &self.define_loc
    }

    pub fn add_reference(&mut self, loc: Location) {
        self.reference_locs.push(loc);
    }

    pub fn reference_locs(&self) -> &[Location] {
        &self.reference_locs
    }

    pub fn kind(&self) -> RecordFieldKind {
        self.kind.clone()
    }

    pub fn r#type(&self) -> &RecordFieldType {
        &self.typ
    }
}

#[derive(Debug, Clone)]
pub enum RecordFieldKind {
    TemplateArg,
    Field,
}

#[derive(Debug, Clone)]
pub enum RecordFieldType {
    Bit,
    Int,
    String,
    Dag,
    Bits(i64),
    List(Box<RecordFieldType>),
    Class(SymbolId, EcoString),
    Code,
}

impl fmt::Display for RecordFieldType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecordFieldType::Bit => write!(f, "bit"),
            RecordFieldType::Int => write!(f, "int"),
            RecordFieldType::String => write!(f, "string"),
            RecordFieldType::Dag => write!(f, "dag"),
            RecordFieldType::Bits(len) => write!(f, "bits<{len}>"),
            RecordFieldType::List(inner_typ) => write!(f, "list<{}>", inner_typ),
            RecordFieldType::Class(_, name) => write!(f, "{}", name),
            RecordFieldType::Code => write!(f, "code"),
        }
    }
}
