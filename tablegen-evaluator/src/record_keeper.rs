use std::collections::HashMap;

use ecow::EcoString;
use id_arena::{Arena, Id};
use tablegen_parser::kind::TokenKind;

#[derive(Debug)]
pub struct RecordKeeper {
    records: Arena<Record>,
    record_map: HashMap<EcoString, RecordId>,
}

impl RecordKeeper {
    pub fn new() -> Self {
        Self {
            records: Arena::new(),
            record_map: HashMap::new(),
        }
    }

    pub fn add_record(&mut self, record: Record) {
        let name = record.name.clone();
        let record_id = self.records.alloc(record);
        self.record_map.insert(name, record_id);
    }

    pub fn find_record(&self, name: &EcoString) -> Option<RecordId> {
        self.record_map.get(name).copied()
    }
}

pub type RecordId = Id<Record>;

#[derive(Debug)]
pub struct Record {
    name: EcoString,
    template_args: HashMap<EcoString, TemplateArg>,
    fields: HashMap<EcoString, RecordField>,
    parents: Vec<RecordRef>,
}

impl Record {
    pub fn new(name: EcoString) -> Self {
        Self {
            name,
            template_args: HashMap::new(),
            fields: HashMap::new(),
            parents: Vec::new(),
        }
    }

    pub fn add_template_arg(&mut self, arg: TemplateArg) {
        self.template_args.insert(arg.name.clone(), arg);
    }

    pub fn add_field(&mut self, field: RecordField) {
        self.fields.insert(field.name.clone(), field);
    }

    pub fn fields(&self) -> impl ExactSizeIterator<Item = &RecordField> {
        self.fields.values()
    }

    pub fn add_parent(&mut self, parent: RecordRef) {
        self.parents.push(parent);
    }
}

#[derive(Debug, Clone)]
pub struct TemplateArg {
    name: EcoString,
    typ: Type,
    initial_value: Value,
}

impl TemplateArg {
    pub fn new(name: EcoString, typ: Type, value: Value) -> Self {
        Self {
            name,
            typ,
            initial_value: value,
        }
    }
}

#[derive(Debug, Clone)]
pub struct RecordField {
    name: EcoString,
    typ: Type,
    value: Value,
}

impl RecordField {
    pub fn new(name: EcoString, typ: Type, value: Value) -> Self {
        Self { name, typ, value }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Bit,
    Int,
    String,
    Dag,
    Bits(i64),
    List(Box<Type>),
    Class(String),
    Code,
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    String(String),
    Code(String),
    Boolean(bool),
    Uninitialized,
    Bits(Vec<Value>),
    List(Vec<Value>),
    Dag(),
    Identifier(String),
    ClassRef(String, Vec<Value>, Vec<(Value, Value)>),
    BangOperator(TokenKind, Vec<Value>),
    CondOperator(Vec<(Value, Value)>),
}
