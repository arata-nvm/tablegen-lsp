use crate::document::DocumentId;
use ecow::EcoString;
use id_arena::Id;
use std::{collections::HashMap, fmt};
use tablegen_parser::parser::TextRange;

pub type Location = (DocumentId, TextRange);

pub type SymbolId = Id<Symbol>;

#[derive(Debug)]
pub enum Symbol {
    Record(Record),
    RecordField(RecordField),
    Variable(Variable),
}

impl Symbol {
    pub fn name(&self) -> &EcoString {
        match self {
            Self::Record(record) => record.name(),
            Self::RecordField(field) => field.name(),
            Self::Variable(variable) => variable.name(),
        }
    }

    pub fn define_loc(&self) -> &Location {
        match self {
            Self::Record(record) => record.define_loc(),
            Self::RecordField(field) => field.define_loc(),
            Self::Variable(variable) => variable.define_loc(),
        }
    }

    pub fn add_reference(&mut self, loc: Location) {
        match self {
            Self::Record(record) => record.add_reference(loc),
            Self::RecordField(field) => field.add_reference(loc),
            Self::Variable(variable) => variable.add_reference(loc),
        }
    }

    pub fn reference_locs(&self) -> &[Location] {
        match self {
            Self::Record(record) => record.reference_locs(),
            Self::RecordField(field) => field.reference_locs(),
            Self::Variable(variable) => variable.reference_locs(),
        }
    }

    pub fn as_record(&self) -> &Record {
        match self {
            Self::Record(record) => record,
            _ => panic!(),
        }
    }

    pub fn as_record_mut(&mut self) -> &mut Record {
        match self {
            Self::Record(record) => record,
            _ => panic!(),
        }
    }

    pub fn as_field(&self) -> &RecordField {
        match self {
            Self::RecordField(field) => field,
            _ => panic!(),
        }
    }

    pub fn as_field_mut(&mut self) -> &mut RecordField {
        match self {
            Self::RecordField(field) => field,
            _ => panic!(),
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
    Defset,
    Defvar,
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
    typ: SymbolType,
}

impl RecordField {
    pub fn new(
        name: EcoString,
        define_loc: Location,
        kind: RecordFieldKind,
        typ: SymbolType,
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

    pub fn r#type(&self) -> &SymbolType {
        &self.typ
    }
}

#[derive(Debug, Clone)]
pub enum RecordFieldKind {
    TemplateArg,
    Field,
}

#[derive(Debug)]
pub struct Variable {
    name: EcoString,
    define_loc: Location,
    reference_locs: Vec<Location>,
    kind: VariableKind,
    typ: SymbolType,
}

#[derive(Debug, Clone)]
pub enum VariableKind {
    Defset,
    Defvar,
}

impl Variable {
    pub fn new(name: EcoString, define_loc: Location, kind: VariableKind, typ: SymbolType) -> Self {
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

    pub fn kind(&self) -> VariableKind {
        self.kind.clone()
    }

    pub fn r#type(&self) -> &SymbolType {
        &self.typ
    }
}

#[derive(Debug, Clone)]
pub enum SymbolType {
    Bit,
    Int,
    String,
    Dag,
    Bits(i64),
    List(Box<SymbolType>),
    Class(SymbolId, EcoString),
    Code,
    Unresolved(EcoString),
}

impl fmt::Display for SymbolType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymbolType::Bit => write!(f, "bit"),
            SymbolType::Int => write!(f, "int"),
            SymbolType::String => write!(f, "string"),
            SymbolType::Dag => write!(f, "dag"),
            SymbolType::Bits(len) => write!(f, "bits<{len}>"),
            SymbolType::List(inner_typ) => write!(f, "list<{}>", inner_typ),
            SymbolType::Class(_, name) => write!(f, "{}", name),
            SymbolType::Code => write!(f, "code"),
            SymbolType::Unresolved(name) => write!(f, "{}", name),
        }
    }
}
