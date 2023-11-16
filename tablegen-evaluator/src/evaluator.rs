use std::collections::HashMap;

use ecow::EcoString;

use crate::record_keeper::{
    Record, RecordField, RecordId, RecordKeeper, RecordRef, TemplateArg, Value,
};

pub(crate) struct Evaluator {
    record_keeper: RecordKeeper,
    current_record: Option<Record>,
    context: Context,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            record_keeper: RecordKeeper::new(),
            current_record: None,
            context: Context::new(),
        }
    }

    pub fn start_record(&mut self, record: Record) {
        self.context.push();
        self.current_record.replace(record);
    }

    pub fn finish_record(&mut self) {
        self.context.pop();
        let record = self.current_record.take().unwrap();
        self.record_keeper.add_record(record);
    }

    pub fn finish_def(&mut self) {
        self.context.pop();
        let record = self.current_record.take().unwrap();
        self.record_keeper.add_def(record);
    }

    pub fn add_record_template_arg(&mut self, arg: TemplateArg) {
        self.current_record.as_mut().unwrap().add_template_arg(arg);
    }

    pub fn add_parent(&mut self, parent: RecordRef) {
        self.current_record.as_mut().unwrap().add_parent(parent);
    }

    pub fn add_record_field(&mut self, field: RecordField) {
        self.current_record.as_mut().unwrap().add_field(field);
    }

    pub fn find_record(&self, name: &EcoString) -> Option<RecordId> {
        self.record_keeper.find_record(name)
    }

    pub fn add_symbol(&mut self, name: EcoString, value: Value) {
        self.context.add_symbol(name, value);
    }

    pub fn find_symbol(&mut self, name: EcoString) -> Option<Value> {
        self.context.find_symbol(name)
    }

    pub fn finish(self) -> RecordKeeper {
        self.record_keeper
    }
}

struct Context(Vec<HashMap<EcoString, Value>>);

impl Context {
    fn new() -> Self {
        Self(vec![HashMap::new()])
    }

    fn add_symbol(&mut self, name: EcoString, value: Value) {
        self.0.last_mut().unwrap().insert(name, value);
    }

    fn find_symbol(&mut self, name: EcoString) -> Option<Value> {
        for scope in self.0.iter().rev() {
            if let Some(value) = scope.get(&name) {
                return Some(value.clone());
            }
        }
        None
    }

    fn push(&mut self) {
        self.0.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.0.pop();
    }
}
