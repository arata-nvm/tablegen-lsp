use async_lsp::lsp_types::{
    ProgressParams, ProgressParamsValue, ProgressToken, WorkDoneProgress, WorkDoneProgressBegin,
    WorkDoneProgressCreateParams, WorkDoneProgressEnd, notification, request,
};
use async_lsp::ClientSocket;

pub struct Progress {
    client: ClientSocket,
    token: ProgressToken,
}

impl Progress {
    pub async fn new(
        client: ClientSocket,
        token: impl Into<String>,
        title: impl Into<String>,
        message: Option<String>,
    ) -> Option<Self> {
        let token = ProgressToken::String(token.into());
        tracing::info!("start work done progress: {:?}", token);

        if let Err(err) = client
            .request::<request::WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
                token: token.clone(),
            })
            .await
        {
            tracing::warn!("failed to create work done progress: {err:?}");
            return None;
        }

        let this = Self { client, token };
        this.notify(WorkDoneProgress::Begin(WorkDoneProgressBegin {
            title: title.into(),
            cancellable: None,
            message,
            percentage: None,
        }));
        Some(this)
    }

    pub fn done(self) {
        std::mem::drop(self);
    }

    fn notify(&self, progress: WorkDoneProgress) {
        if let Err(err) = self
            .client
            .notify::<notification::Progress>(ProgressParams {
                token: self.token.clone(),
                value: ProgressParamsValue::WorkDone(progress),
            })
        {
            tracing::warn!("failed to notify progress: {err:?}");
        }
    }
}

impl Drop for Progress {
    fn drop(&mut self) {
        tracing::info!("end work done progress: {:?}", self.token);
        self.notify(WorkDoneProgress::End(WorkDoneProgressEnd { message: None }));
    }
}
