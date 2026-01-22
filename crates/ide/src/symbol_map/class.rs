use ecow::EcoString;
use id_arena::Id;
use indexmap::IndexMap;

use crate::file_system::FileRange;

use super::{
    record::{AsRecordData, AsRecordDataMut, RecordData},
    template_arg::TemplateArgumentId,
};

pub type ClassId = Id<Class>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Class {
    inner: RecordData,
    name_to_template_arg: IndexMap<EcoString, TemplateArgumentId>,
}

impl Class {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            inner: RecordData::new(name, define_loc),
            name_to_template_arg: IndexMap::new(),
        }
    }

    pub fn add_template_arg(&mut self, name: EcoString, template_arg_id: TemplateArgumentId) {
        self.name_to_template_arg.insert(name, template_arg_id);
    }

    pub fn iter_template_arg(&self) -> impl Iterator<Item = TemplateArgumentId> + '_ {
        self.name_to_template_arg.values().copied()
    }

    pub fn find_template_arg(&self, name: &EcoString) -> Option<TemplateArgumentId> {
        self.name_to_template_arg.get(name).copied()
    }

    pub fn is_empty(&self) -> bool {
        self.name_to_template_arg.is_empty()
            && self.iter_direct_field().count() == 0
            && self.parent_classes().is_empty()
    }
}

impl AsRecordData for Class {
    fn record_data(&self) -> &RecordData {
        &self.inner
    }
}

impl AsRecordDataMut for Class {
    fn record_data_mut(&mut self) -> &mut RecordData {
        &mut self.inner
    }
}
