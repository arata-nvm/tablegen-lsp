use std::path::Path;

use ide::file_system::FilePath;
use serde_json::Value;

pub const CONFIG_SECTION: &str = "tablegen-lsp";

#[derive(Debug, Default)]
pub struct Config {
    pub include_dirs: Vec<FilePath>,
    pub source_root_path: Option<FilePath>,
}

impl Config {
    pub fn update(&mut self, mut v: Value) -> Result<(), String> {
        if let Some(v) = v.pointer_mut("/includePath") {
            let v: Vec<String> = match serde_json::from_value(v.take()) {
                Ok(v) => v,
                Err(_) => return Err("invalid value of tablegen-lsp.includePath".into()),
            };

            self.include_dirs = v
                .into_iter()
                .map(|s| FilePath::from(Path::new(&s)))
                .collect();
        }

        if let Some(v) = v.pointer_mut("/sourceRootPath") {
            let v: Option<String> = match serde_json::from_value(v.take()) {
                Ok(v) => v,
                Err(_) => return Err("invalid value of tablegen-lsp.sourceRootPath".into()),
            };

            self.source_root_path = v.map(|s| FilePath::from(Path::new(&s)));
        }

        Ok(())
    }
}
