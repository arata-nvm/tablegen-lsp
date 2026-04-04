use ide::{
    analysis::AnalysisHost,
    file_system::{FileId, FilePath, FilePosition, FileRange, FileSet, FileSystem},
    interop,
};
use std::{
    fs,
    sync::Arc,
    time::{Duration, Instant},
};
use syntax::parser::{TextRange, TextSize};
use tracing_subscriber::EnvFilter;

fn measure_time<T>(f: impl FnOnce() -> T) -> (Duration, T) {
    let start = Instant::now();
    let retval = f();
    (start.elapsed(), retval)
}

fn print_usage(prog: &str) -> ! {
    eprintln!(
        "Usage: {prog} [--include-dir path] <input file> <method> [args...]

Methods:
  list-file                 List all files in the source unit
  diag                      Show diagnostics
  doc-symbol                Show document symbols
  doc-link                  Show document links
  doc-fold                  Show folding ranges
  hover <line> <col>        Show hover info at position
  definition <line> <col>   Go to definition at position
  completion <line> <col>   Show completions at position
  references <line> <col>   Show references at position
  inlay-hint                Show inlay hints
  all                       Run all methods (uses 0:0 for position-based)"
    );
    std::process::exit(1);
}

pub fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

    let args: Vec<String> = std::env::args().collect();
    let mut i = 1;

    let mut include_dirs = vec![];
    if args.get(i).map(|s| s.as_str()) == Some("--include-dir") {
        i += 1;
        let Some(include_path) = args.get(i) else {
            print_usage(&args[0]);
        };
        include_dirs.push(FilePath(include_path.into()));
        i += 1;
    }

    let Some(input_file) = args.get(i) else {
        print_usage(&args[0]);
    };
    i += 1;

    let Some(method) = args.get(i) else {
        print_usage(&args[0]);
    };
    i += 1;

    let remaining_args: Vec<&str> = args[i..].iter().map(|s| s.as_str()).collect();

    let mut host = AnalysisHost::new();
    let mut vfs = Vfs::new();

    let file_path = FilePath(input_file.into());
    let file_id = vfs.assign_or_get_file_id(file_path.clone());
    let file_content = vfs.read_content(&file_path).expect("failed to read file");
    host.set_file_content(file_id, Arc::from(file_content.as_str()));
    let source_unit_id = host
        .load_source_unit(&mut vfs, file_id, &include_dirs)
        .expect("failed to load source unit");

    let (_, result) = measure_time(|| {
        interop::parse_source_unit_with_tblgen(&file_path, &include_dirs, &vfs)
            .expect("failed to parse source unit")
    });
    host.set_tblgen_parse_result(source_unit_id, result);

    let analysis = host.analysis();
    let line_index = analysis
        .line_index(file_id)
        .expect("failed to get line index");

    let parse_pos = |args: &[&str]| -> FilePosition {
        if args.len() < 2 {
            eprintln!("Error: <line> <col> arguments required");
            std::process::exit(1);
        }
        let line: usize = args[0].parse().expect("invalid line number");
        let col: u32 = args[1].parse().expect("invalid column number");
        let offset = line_index.line_to_pos(line) + TextSize::from(col);
        FilePosition::new(file_id, offset)
    };

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
            let (_, diags) = measure_time(|| {
                analysis
                    .diagnostics(source_unit_id)
                    .expect("failed to get diagnostics")
            });
            for diag in diags {
                println!("{diag:?}");
            }
        }
        "doc-symbol" => {
            let symbols = analysis
                .document_symbol(file_id)
                .expect("failed to get document symbols")
                .unwrap();
            for symbol in symbols {
                println!("{symbol:?}");
            }
        }
        "doc-link" => {
            let links = analysis
                .document_link(source_unit_id, file_id)
                .expect("failed to get document links")
                .unwrap();
            for link in links {
                println!("{link:?}");
            }
        }
        "doc-fold" => {
            let ranges = analysis
                .folding_range(file_id)
                .expect("failed to get folding ranges")
                .unwrap();
            for range in ranges {
                println!("{range:?}");
            }
        }
        "hover" => {
            let pos = parse_pos(&remaining_args);
            match analysis
                .hover(source_unit_id, pos)
                .expect("failed to get hover")
            {
                Some(hover) => println!("{hover:?}"),
                None => println!("(no hover info)"),
            }
        }
        "definition" => {
            let pos = parse_pos(&remaining_args);
            match analysis
                .goto_definition(source_unit_id, pos)
                .expect("failed to get definition")
            {
                Some(location) => {
                    let path = vfs.path_for_file(&location.file);
                    println!("{path:?} {location:?}");
                }
                None => println!("(no definition)"),
            }
        }
        "completion" => {
            let pos = parse_pos(&remaining_args);
            match analysis
                .completion(source_unit_id, pos, None)
                .expect("failed to get completions")
            {
                Some(items) => {
                    for item in items {
                        println!("{item:?}");
                    }
                }
                None => println!("(no completions)"),
            }
        }
        "references" => {
            let pos = parse_pos(&remaining_args);
            match analysis
                .references(source_unit_id, pos)
                .expect("failed to get references")
            {
                Some(locations) => {
                    for location in locations {
                        let path = vfs.path_for_file(&location.file);
                        println!("{path:?} {location:?}");
                    }
                }
                None => println!("(no references)"),
            }
        }
        "inlay-hint" => {
            let file_range = FileRange::new(
                file_id,
                TextRange::new(
                    TextSize::from(0),
                    TextSize::try_from(file_content.len()).unwrap(),
                ),
            );
            match analysis
                .inlay_hint(source_unit_id, file_range)
                .expect("failed to get inlay hints")
            {
                Some(hints) => {
                    for hint in hints {
                        println!("{hint:?}");
                    }
                }
                None => println!("(no inlay hints)"),
            }
        }
        "all" => {
            let pos = if remaining_args.len() >= 2 {
                parse_pos(&remaining_args)
            } else {
                FilePosition::new(file_id, TextSize::from(0))
            };
            let file_range = FileRange::new(
                file_id,
                TextRange::new(
                    TextSize::from(0),
                    TextSize::try_from(file_content.len()).unwrap(),
                ),
            );

            println!("=== diag ===");
            match analysis.diagnostics(source_unit_id) {
                Ok(diags) => {
                    for diag in diags {
                        println!("{diag:?}");
                    }
                }
                Err(e) => eprintln!("error: {e}"),
            }

            println!("\n=== doc-symbol ===");
            match analysis.document_symbol(file_id) {
                Ok(Some(symbols)) => {
                    for symbol in symbols {
                        println!("{symbol:?}");
                    }
                }
                Ok(None) => println!("(none)"),
                Err(e) => eprintln!("error: {e}"),
            }

            println!("\n=== doc-link ===");
            match analysis.document_link(source_unit_id, file_id) {
                Ok(Some(links)) => {
                    for link in links {
                        println!("{link:?}");
                    }
                }
                Ok(None) => println!("(none)"),
                Err(e) => eprintln!("error: {e}"),
            }

            println!("\n=== doc-fold ===");
            match analysis.folding_range(file_id) {
                Ok(Some(ranges)) => {
                    for range in ranges {
                        println!("{range:?}");
                    }
                }
                Ok(None) => println!("(none)"),
                Err(e) => eprintln!("error: {e}"),
            }

            println!("\n=== hover ({pos:?}) ===");
            match analysis.hover(source_unit_id, pos) {
                Ok(Some(hover)) => println!("{hover:?}"),
                Ok(None) => println!("(none)"),
                Err(e) => eprintln!("error: {e}"),
            }

            println!("\n=== definition ({pos:?}) ===");
            match analysis.goto_definition(source_unit_id, pos) {
                Ok(Some(location)) => {
                    let path = vfs.path_for_file(&location.file);
                    println!("{path:?} {location:?}");
                }
                Ok(None) => println!("(none)"),
                Err(e) => eprintln!("error: {e}"),
            }

            println!("\n=== completion ({pos:?}) ===");
            match analysis.completion(source_unit_id, pos, None) {
                Ok(Some(items)) => {
                    for item in items {
                        println!("{item:?}");
                    }
                }
                Ok(None) => println!("(none)"),
                Err(e) => eprintln!("error: {e}"),
            }

            println!("\n=== references ({pos:?}) ===");
            match analysis.references(source_unit_id, pos) {
                Ok(Some(locations)) => {
                    for location in locations {
                        let path = vfs.path_for_file(&location.file);
                        println!("{path:?} {location:?}");
                    }
                }
                Ok(None) => println!("(none)"),
                Err(e) => eprintln!("error: {e}"),
            }

            println!("\n=== inlay-hint ===");
            match analysis.inlay_hint(source_unit_id, file_range) {
                Ok(Some(hints)) => {
                    for hint in hints {
                        println!("{hint:?}");
                    }
                }
                Ok(None) => println!("(none)"),
                Err(e) => eprintln!("error: {e}"),
            }
        }
        _ => {
            eprintln!("unknown method: {method}");
            print_usage(&args[0]);
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
                tracing::info!("assign file id: {path:?} -> {file_id:?}",);
                self.file_set.insert(file_id, path);
                file_id
            }
        }
    }

    fn file_for_path(&self, path: &FilePath) -> Option<FileId> {
        self.file_set.file_for_path(path)
    }

    fn path_for_file(&self, file_id: &FileId) -> FilePath {
        self.file_set.path_for_file(file_id)
    }

    fn read_content(&self, file_path: &FilePath) -> Option<String> {
        fs::read_to_string(&file_path.0).ok()
    }
}
