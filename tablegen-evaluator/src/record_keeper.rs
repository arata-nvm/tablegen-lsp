use std::collections::HashMap;

use ecow::EcoString;
use tablegen_parser::kind::TokenKind;

#[derive(Debug)]
pub struct RecordKeeper {
    records: HashMap<EcoString, Record>,
}

impl RecordKeeper {
    pub fn new() -> Self {
        Self {
            records: HashMap::new(),
        }
    }

    pub fn add_record(&mut self, record: Record) {
        self.records.insert(record.name.clone(), record);
    }

    pub fn find_record(&self, name: &EcoString) -> Option<&Record> {
        self.records.get(name)
    }
}

#[derive(Debug, Clone)]
pub struct Record {
    name: EcoString,
    template_args: HashMap<EcoString, TemplateArg>,
    fields: HashMap<EcoString, RecordField>,
}

impl Record {
    pub fn new(name: EcoString) -> Self {
        Self {
            name,
            template_args: HashMap::new(),
            fields: HashMap::new(),
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
