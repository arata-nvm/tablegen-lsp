use std::{fs, sync::Arc};

use ide::{
    analysis::AnalysisHost,
    file_system::{FileId, FilePath, FileSet, FileSystem},
    symbol_map::symbol::Symbol,
};

pub fn main() {
    let mut args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut host = AnalysisHost::new();
    let mut vfs = Vfs::new();

    let file_path = FilePath(std::mem::take(&mut args[1]).into());
    let file_id = vfs.assign_or_get_file_id(file_path.clone());
    let file_content = vfs.read_content(&file_path).expect("failed to read file");
    host.set_file_content(file_id, Arc::from(file_content.as_str()));
    host.set_root_file(&mut vfs, file_id);

    let index = host.analysis().index();
    for diag in index.diagnostics() {
        println!("{:?}", diag);
    }

    let symbol_map = index.symbol_map();

    let Some(iter) = symbol_map.iter_symbols_in_file(file_id) else {
        return;
    };

    println!("------------- Classes -----------------");
    for symbol_id in iter {
        let Symbol::Class(class) = symbol_map.symbol(symbol_id) else {
            continue;
        };
        let template_args = class
            .iter_template_arg()
            .map(|id| symbol_map.template_arg(id))
            .map(|arg| format!("{} {}", arg.typ, arg.name))
            .collect::<Vec<String>>()
            .join(", ");
        let parent_classes = class
            .parent_list
            .iter()
            .map(|&id| symbol_map.class(id))
            .map(|class| format!("{}", class.name))
            .collect::<Vec<String>>()
            .join(", ");
        println!(
            "class {}<{}> : {} {{",
            class.name, template_args, parent_classes
        );
        for field_id in class.iter_field() {
            let field = symbol_map.record_field(field_id);
            println!("  {} {};", field.typ, field.name);
        }
        println!("}}");
    }

    let iter = symbol_map
        .iter_symbols_in_file(file_id)
        .expect("failed to get symbols");

    println!("------------- Defs -----------------");
    for symbol_id in iter {
        let Symbol::Def(def) = symbol_map.symbol(symbol_id) else {
            continue;
        };
        let parent_classes = def
            .parent_list
            .iter()
            .map(|&id| symbol_map.class(id))
            .map(|class| format!("{}", class.name))
            .collect::<Vec<String>>()
            .join(", ");
        println!("def {} {{ // {}", def.name, parent_classes);
        for field_id in def.iter_field() {
            let field = symbol_map.record_field(field_id);
            println!("  {} {};", field.typ, field.name);
        }
        println!("}}");
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
        let Ok(content) = fs::read_to_string(&file_path.0) else {
            tracing::info!("failed to read file: file_path={file_path:?}");
            return None;
        };

        Some(content)
    }
}
