use crate::record_keeper::{Record, RecordField, RecordKeeper, TemplateArg};

pub(crate) struct Evaluator {
    record_keeper: RecordKeeper,
    current_record: Option<Record>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            record_keeper: RecordKeeper::new(),
            current_record: None,
        }
    }

    pub fn set_current_record(&mut self, record: Record) {
        self.current_record.replace(record);
    }

    pub fn add_record_template_arg(&mut self, arg: TemplateArg) {
        self.current_record.as_mut().unwrap().add_template_arg(arg);
    }

    pub fn add_record_field(&mut self, field: RecordField) {
        self.current_record.as_mut().unwrap().add_field(field);
    }

    pub fn finish_record(&mut self) {
        let record = self.current_record.take().unwrap();
        self.record_keeper.add_record(record);
    }

    pub fn finish(self) -> RecordKeeper {
        self.record_keeper
    }
}
