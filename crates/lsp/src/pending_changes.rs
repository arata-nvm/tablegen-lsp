use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use async_lsp::ClientSocket;
use tokio::task;

use ide::analysis::AnalysisHost;
use ide::file_system::FileId;

use crate::server::DebounceTimerEvent;

const DEBOUNCE_DELAY: Duration = Duration::from_millis(200);

pub struct PendingChanges {
    file_changes: HashMap<FileId, Arc<str>>,
    debounce_file_id: Option<FileId>,
    debounce_task: Option<task::JoinHandle<()>>,
}

impl Default for PendingChanges {
    fn default() -> Self {
        Self::new()
    }
}

impl PendingChanges {
    pub fn new() -> Self {
        Self {
            file_changes: HashMap::new(),
            debounce_file_id: None,
            debounce_task: None,
        }
    }

    pub fn enqueue(&mut self, file_id: FileId, content: Arc<str>) {
        self.file_changes.insert(file_id, content);
    }

    pub fn schedule_debounce(&mut self, file_id: FileId, client: ClientSocket) {
        self.debounce_file_id = Some(file_id);
        if let Some(old) = self.debounce_task.take() {
            old.abort();
        }
        self.debounce_task = Some(task::spawn(async move {
            tokio::time::sleep(DEBOUNCE_DELAY).await;
            if let Err(err) = client.emit(DebounceTimerEvent) {
                tracing::warn!("failed to emit debounce timer event: {err:?}");
            }
        }));
    }

    pub fn cancel_for_file(&mut self, file_id: FileId) {
        self.file_changes.remove(&file_id);
        if self.debounce_file_id == Some(file_id) {
            self.debounce_file_id = None;
            if let Some(task) = self.debounce_task.take() {
                task.abort();
            }
        }
    }

    pub fn apply_to(&mut self, host: &mut AnalysisHost) -> bool {
        if self.file_changes.is_empty() {
            return false;
        }
        tracing::info!(
            "flushing {} pending file change(s)",
            self.file_changes.len()
        );
        for (file_id, content) in self.file_changes.drain() {
            host.set_file_content(file_id, content);
        }
        true
    }

    pub fn take_debounce_file_id(&mut self) -> Option<FileId> {
        self.debounce_file_id.take()
    }

    pub fn has_changes(&self) -> bool {
        !self.file_changes.is_empty()
    }
}
