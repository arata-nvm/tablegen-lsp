use ecow::EcoString;
use id_arena::Id;

use crate::file_system::FileRange;

use super::record::{AsRecordData, AsRecordDataMut, RecordData};

pub type DefId = Id<Def>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Def {
    inner: RecordData,
}

impl Def {
    pub fn new(name: EcoString, define_loc: FileRange) -> Self {
        Self {
            inner: RecordData::new(name, define_loc),
        }
    }

    pub fn clone_with(&self, name: EcoString, define_loc: FileRange) -> Self {
        Self {
            inner: self.inner.clone_with(name, define_loc),
        }
    }
}

impl AsRecordData for Def {
    fn record_data(&self) -> &RecordData {
        &self.inner
    }
}

impl AsRecordDataMut for Def {
    fn record_data_mut(&mut self) -> &mut RecordData {
        &mut self.inner
    }
}
