use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use ide::file_system::SourceUnitId;
use ide::index::Index;

pub struct SourceUnitManager {
    opened: HashSet<SourceUnitId>,
    root: Option<SourceUnitId>,
    latest_indices: HashMap<SourceUnitId, Arc<Index>>,
}

impl Default for SourceUnitManager {
    fn default() -> Self {
        Self::new()
    }
}

impl SourceUnitManager {
    pub fn new() -> Self {
        Self {
            opened: HashSet::new(),
            root: None,
            latest_indices: HashMap::new(),
        }
    }

    pub fn open(&mut self, id: SourceUnitId) {
        self.opened.insert(id);
    }

    pub fn close(&mut self, id: SourceUnitId) {
        self.opened.remove(&id);
    }

    pub fn set_root(&mut self, id: SourceUnitId) {
        self.root = Some(id);
    }

    pub fn clear_root(&mut self) {
        self.root = None;
    }

    pub fn root(&self) -> Option<SourceUnitId> {
        self.root
    }

    pub fn active(&self) -> HashSet<SourceUnitId> {
        match self.root {
            Some(id) => HashSet::from([id]),
            None => self.opened.clone(),
        }
    }

    pub fn update_index(&mut self, id: SourceUnitId, index: Arc<Index>) {
        self.latest_indices.insert(id, index);
    }

    pub fn latest_indices(&self) -> &HashMap<SourceUnitId, Arc<Index>> {
        &self.latest_indices
    }
}
