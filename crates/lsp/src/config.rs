use std::path::PathBuf;

pub const CONFIG_KEY: &str = "tablegen-lsp";

#[derive(Debug, Clone)]
pub struct Config {
    pub root_path: PathBuf,
    pub include_path: Vec<PathBuf>,
}

impl Config {
    pub fn new(root_path: PathBuf) -> Self {
        Self {
            root_path,
            include_path: vec![],
        }
    }

    pub fn update(&mut self, mut options: serde_json::Value) {
        if let Some(v) = options.pointer_mut("/includePath") {
            match serde_json::from_value(v.take()) {
                Ok(v) => self.include_path = v,
                Err(err) => {}
            }
        }
    }
}
