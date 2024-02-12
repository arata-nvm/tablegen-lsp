use std::path::PathBuf;

#[derive(Debug, Default)]
pub struct Config {
    pub include_path: Vec<PathBuf>,
}
