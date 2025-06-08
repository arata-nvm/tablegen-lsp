use ide::{
    analysis::AnalysisHost,
    file_system::{FileId, FilePath, FileSet, FileSystem},
};
use std::{fs, sync::Arc};
use tracing_subscriber::EnvFilter;

pub fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

    let mut args: Vec<String> = std::env::args().collect();
    let (include_dirs, input_file, method) = match args.len() {
        3 => {
            let file = args.remove(1);
            let method = args.remove(1);
            (vec![], file, method)
        }
        5 if args[1] == "--include-dir" => {
            let _ = args.remove(1);
            let include_path = args.remove(1);
            let file = args.remove(1);
            let method = args.remove(1);
            (vec![FilePath(include_path.into())], file, method)
        }
        _ => {
            eprintln!(
                "Usage: {} [--include-dir path] <input file> <method>",
                args[0]
            );
            std::process::exit(1);
        }
    };

    let mut host = AnalysisHost::new();
    let mut vfs = Vfs::new();

    let file_path = FilePath(input_file.into());
    let file_id = vfs.assign_or_get_file_id(file_path.clone());
    let file_content = vfs.read_content(&file_path).expect("failed to read file");
    host.set_file_content(file_id, Arc::from(file_content.as_str()));
    let source_unit_id = host.load_source_unit(&mut vfs, file_id, &include_dirs);

    let analysis = host.analysis();
    match method.as_str() {
        "list-file" => {
            let mut file_ids = vfs.file_set.iter_files().collect::<Vec<_>>();
            file_ids.sort();
            for file_id in file_ids {
                let file_path = vfs.path_for_file(&file_id);
                println!("{file_id:?}: {file_path:?}");
            }
        }
        "diag" => {
            let all_diags = analysis.diagnostics(source_unit_id);
            for (file, diags) in all_diags {
                let path = vfs.path_for_file(&file);
                println!("{path:?}:");
                for diag in diags {
                    println!("  {diag:?}");
                }
            }
        }
        "doc-symbol" => {
            let symbols = analysis.document_symbol(source_unit_id, file_id).unwrap();
            for symbol in symbols {
                println!("{symbol:?}");
            }
        }
        "doc-link" => {
            let links = analysis.document_link(source_unit_id, file_id).unwrap();
            for link in links {
                println!("{link:?}");
            }
        }
        "doc-fold" => {
            let ranges = analysis.folding_range(file_id).unwrap();
            for range in ranges {
                println!("{range:?}");
            }
        }
        _ => {
            panic!("unknown method");
        }
    }
}

#[derive(Debug, Default)]
pub struct Vfs {
    file_set: FileSet,
    next_file_id: u32,
}

impl Vfs {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn file_for_path(&self, path: &FilePath) -> Option<FileId> {
        self.file_set.file_for_path(path)
    }

    fn alloc_file_id(&mut self) -> FileId {
        let file_id = FileId(self.next_file_id);
        self.next_file_id += 1;
        file_id
    }
}

impl FileSystem for Vfs {
    fn assign_or_get_file_id(&mut self, path: FilePath) -> FileId {
        match self.file_set.file_for_path(&path) {
            Some(file_id) => file_id,
            None => {
                let file_id = self.alloc_file_id();
                tracing::debug!("assign file id: {path:?} -> {file_id:?}",);
                self.file_set.insert(file_id, path);
                file_id
            }
        }
    }

    fn path_for_file(&self, file_id: &FileId) -> &FilePath {
        self.file_set.path_for_file(file_id)
    }

    fn read_content(&self, file_path: &FilePath) -> Option<String> {
        fs::read_to_string(&file_path.0).ok()
    }
}
