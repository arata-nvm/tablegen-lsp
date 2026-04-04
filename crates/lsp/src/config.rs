use std::path::Path;

use ide::file_system::FilePath;
use serde_json::Value;

pub const CONFIG_SECTION: &str = "tablegen-lsp";

#[derive(Debug, Default)]
pub struct Config {
    pub include_dirs: Vec<FilePath>,
    pub default_source_root_path: Option<FilePath>,
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

        if let Some(v) = v.pointer_mut("/defaultSourceRootPath") {
            let v: Option<String> = match serde_json::from_value(v.take()) {
                Ok(v) => v,
                Err(_) => return Err("invalid value of tablegen-lsp.defaultSourceRootPath".into()),
            };

            self.default_source_root_path = v
                .filter(|s| !s.is_empty())
                .map(|s| FilePath::from(Path::new(&s)));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::Config;

    #[test]
    fn empty_default_source_root_path() {
        let mut config = Config::default();
        config
            .update(json!({
                "defaultSourceRootPath": "",
            }))
            .unwrap();

        assert!(config.default_source_root_path.is_none());
    }

    #[test]
    fn non_empty_default_source_root_path() {
        let mut config = Config::default();
        config
            .update(json!({
                "defaultSourceRootPath": "/root.td",
            }))
            .unwrap();

        assert_eq!(
            config.default_source_root_path.unwrap().to_str(),
            "/root.td"
        );
    }
}
