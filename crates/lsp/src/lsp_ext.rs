use async_lsp::lsp_types::{Url, notification::Notification};
use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub enum SetSourceRoot {}

impl Notification for SetSourceRoot {
    type Params = SetSourceRootParams;
    const METHOD: &'static str = "tablegenLsp/setSourceRoot";
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SetSourceRootParams {
    pub uri: Url,
}

#[derive(Debug)]
pub enum ClearSourceRoot {}

impl Notification for ClearSourceRoot {
    type Params = ClearSourceRootParams;
    const METHOD: &'static str = "tablegenLsp/clearSourceRoot";
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ClearSourceRootParams {}
