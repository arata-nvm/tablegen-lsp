use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use ide::{
    file_system::SourceUnitId,
    index::Index,
};

/// Thread-safe storage for background-computed indexes.
///
/// This is intentionally very small in scope:
/// - It only stores the latest completed `Index` per `SourceUnitId`.
/// - It does not perform any scheduling by itself; callers are responsible
///   for spawning background tasks and calling `set`.
/// - Callers are also responsible for deciding when an index is “stale enough”
///   and enqueueing a new computation.
#[derive(Clone, Default)]
pub struct BackgroundIndex {
    inner: Arc<Inner>,
}

#[derive(Default)]
struct Inner {
    indexes: RwLock<HashMap<SourceUnitId, Arc<Index>>>,
}

impl BackgroundIndex {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the latest completed index for the given source unit, if any.
    ///
    /// The result may be slightly stale w.r.t. the most recent edits, but
    /// that is acceptable for fast, interactive features.
    pub fn get(&self, id: SourceUnitId) -> Option<Arc<Index>> {
        self.inner
            .indexes
            .read()
            .ok()
            .and_then(|map| map.get(&id).cloned())
    }

    /// Store a newly computed index for the given source unit.
    ///
    /// This simply replaces any previous snapshot; callers use higher-level
    /// policies (e.g. enqueueing jobs on did_change / did_save) to keep it
    /// reasonably fresh.
    pub fn set(&self, id: SourceUnitId, index: Arc<Index>) {
        if let Ok(mut map) = self.inner.indexes.write() {
            map.insert(id, index);
        }
    }
}

