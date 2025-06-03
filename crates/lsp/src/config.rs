use std::path::Path;

use ide::file_system::FilePath;
use serde_json::Value;

pub const CONFIG_SECTION: &str = "tablegen-lsp";

#[derive(Debug, Default)]
pub struct Config {
    pub include_dirs: Vec<FilePath>,
}

impl Config {
    pub fn update(&mut self, mut v: Value) -> Result<(), String> {
        let Some(v) = v.pointer_mut("/includePaths") else {
            return Ok(());
        };
        let v: Vec<String> = match serde_json::from_value(v.take()) {
            Ok(v) => v,
            Err(_) => return Err("invalid value of tablegen-lsp.includePaths".into()),
        };

        self.include_dirs = v
            .into_iter()
            .map(|s| FilePath::from(Path::new(&s)))
            .collect();

        Ok(())
    }
}
